/* Golden-master interop: build the same message two ways -- with the
 * reference C encoder (opensourcerouting/c-capnproto) and with our Fortran
 * runtime through the capnp_cabi shim -- and assert the framed wire bytes
 * are identical. A cross-decode test then reads each encoder's output with
 * the other decoder, and a packing test checks c-capnproto's deflate against
 * the spec vector.
 *
 * Schema under test (built by hand, no generated code):
 *   root :Struct {
 *     value @0 :UInt32;            # data offset 0
 *     name  @0p :Text;             # pointer slot 0
 *     items @1p :List(Elem);       # pointer slot 1, composite, 2 elements
 *   }
 *   Elem :Struct {
 *     n @0 :UInt32;                # data offset 0 (one data word)
 *     _  @0p :AnyPointer;          # spare pointer slot, left null
 *   }
 *
 * Why Elem carries a spare (null) pointer slot: c-capnproto's capn_new_list
 * only emits a COMPOSITE list when `ptrs || datasz > 8` (see capn.c
 * capn_new_list). A struct with a single data word and no pointers would be
 * down-encoded to a primitive List(UInt64) there, while our runtime always
 * emits composite. Giving Elem one pointer slot forces composite on both
 * sides so the golden bytes can match. The u32 field @0 is exactly as
 * specified; the spare slot stays zero and costs nothing on the wire beyond
 * the (shared) pointer word both encoders reserve.
 */

#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <stdint.h>
#include <string.h>
#include <cmocka.h>

#include "capnp_c.h"
#include "capnp_priv.h" /* struct capn_stream, capn_deflate (internal) */

/* ---- capnp_cabi shim (Fortran, bind(c)) ---------------------------------- */
extern int cabi_builder_new(void);
extern void cabi_builder_free(int h);
extern int cabi_new_struct(int h, int dwords, int pwords);
extern int cabi_new_composite_list(int h, int count, int dwords, int pwords);
extern int cabi_list_get_struct(int h, int list_id, int i);
extern int cabi_set_root(int h, int obj_id);
extern int cabi_setp(int h, int obj_id, int slot, int child_id);
extern int cabi_set_u32(int h, int obj_id, int byte_off, int32_t value);
extern int cabi_set_text(int h, int obj_id, int slot, const char *str);
extern int cabi_serialize(int h, void *buf, int64_t cap, int64_t *written);
extern int cabi_deserialize(const void *buf, int64_t len);
extern int cabi_root(int h);
extern int32_t cabi_get_u32(int h, int obj_id, int byte_off);
extern int cabi_getp(int h, int obj_id, int slot);
extern int cabi_get_text(int h, int obj_id, int slot, void *buf, int64_t cap, int64_t *written);
extern int64_t cabi_list_len(int h, int list_id);
extern int cabi_new_list(int h, int esize, int64_t count);
extern int cabi_list_set_i32(int h, int list_id, int64_t i, int32_t value);
extern int32_t cabi_list_get_i32(int h, int list_id, int64_t i);
extern int cabi_serialize_packed(int h, void *buf, int64_t cap, int64_t *written);
extern int cabi_deserialize_packed(const void *buf, int64_t len);
extern int cabi_canonicalize(int h, void *buf, int64_t cap, int64_t *written);

#define VAL_ROOT 42u
#define VAL_E0 100u
#define VAL_E1 200u
#define NAME "hi"

/* Build the message with the Fortran runtime via the shim. Returns the framed
 * length, or a negative capnp error code. Allocation order: root struct, then
 * name text, then the composite list -- mirrored on the C side below. */
static int64_t build_cabi(uint8_t *buf, int64_t cap)
{
	int h, rs, lst, e0, e1, rc;
	int64_t written = 0;

	h = cabi_builder_new();
	if (h < 0)
		return -100;

	rs = cabi_new_struct(h, 1, 2); /* 1 data word, 2 pointers */
	cabi_set_u32(h, rs, 0, (int32_t)VAL_ROOT);
	cabi_set_text(h, rs, 0, NAME);

	lst = cabi_new_composite_list(h, 2, 1, 1); /* 2 elems, 1 dword, 1 pword */
	e0 = cabi_list_get_struct(h, lst, 0);
	cabi_set_u32(h, e0, 0, (int32_t)VAL_E0);
	e1 = cabi_list_get_struct(h, lst, 1);
	cabi_set_u32(h, e1, 0, (int32_t)VAL_E1);

	cabi_setp(h, rs, 1, lst);
	cabi_set_root(h, rs);

	rc = cabi_serialize(h, buf, cap, &written);
	cabi_builder_free(h);
	if (rc != 0)
		return -(int64_t)rc;
	return written;
}

/* Build the identical message with the reference c-capnproto encoder, in the
 * same allocation order. Returns the framed length. */
static int64_t build_capn(uint8_t *buf, int64_t cap)
{
	struct capn c;
	capn_ptr root, rs, lst, e0, e1;
	capn_text t;
	int64_t n;

	capn_init_malloc(&c);
	root = capn_root(&c);

	rs = capn_new_struct(root.seg, 8, 2); /* datasz bytes = 8, ptrs = 2 */
	capn_write32(rs, 0, VAL_ROOT);

	memset(&t, 0, sizeof t);
	t.str = NAME;
	t.len = (int)strlen(NAME);
	capn_set_text(rs, 0, t);

	lst = capn_new_list(root.seg, 2, 8, 1); /* len 2, datasz 8, ptrs 1 -> composite */
	e0 = capn_getp(lst, 0, 0);
	capn_write32(e0, 0, VAL_E0);
	e1 = capn_getp(lst, 1, 0);
	capn_write32(e1, 0, VAL_E1);

	capn_setp(rs, 1, lst);
	capn_setp(root, 0, rs);

	n = capn_write_mem(&c, buf, (size_t)cap, 0);
	capn_free(&c);
	return n;
}

static void test_golden_bytes(void **state)
{
	uint8_t fbuf[512], cbuf[512];
	int64_t fn, cn;
	(void)state;

	fn = build_cabi(fbuf, sizeof fbuf);
	cn = build_capn(cbuf, sizeof cbuf);

	assert_true(fn > 0);
	assert_true(cn > 0);
	/* The framed length must match first; a size mismatch means the two
	 * encoders disagree on segment layout (see the schema comment above on
	 * the composite-list gate) rather than on individual field bytes. */
	assert_int_equal(fn, cn);
	assert_memory_equal(fbuf, cbuf, (size_t)cn);
}

static void test_cross_decode(void **state)
{
	uint8_t fbuf[512], cbuf[512];
	int64_t fn, cn, tn;
	(void)state;

	fn = build_cabi(fbuf, sizeof fbuf);
	cn = build_capn(cbuf, sizeof cbuf);
	assert_true(fn > 0);
	assert_true(cn > 0);

	/* c-capnproto bytes -> our decoder (via the shim). */
	{
		int rh, rroot, rlist, re0, re1;
		char tb[64];

		rh = cabi_deserialize(cbuf, cn);
		assert_true(rh >= 1);
		rroot = cabi_root(rh);
		assert_true(rroot >= 1);
		assert_int_equal((uint32_t)cabi_get_u32(rh, rroot, 0), VAL_ROOT);

		assert_int_equal(cabi_get_text(rh, rroot, 0, tb, sizeof tb, &tn), 0);
		assert_int_equal(tn, (int64_t)strlen(NAME));
		tb[tn] = '\0';
		assert_string_equal(tb, NAME);

		rlist = cabi_getp(rh, rroot, 1);
		assert_true(rlist >= 1);
		assert_int_equal(cabi_list_len(rh, rlist), 2);
		re0 = cabi_list_get_struct(rh, rlist, 0);
		assert_int_equal((uint32_t)cabi_get_u32(rh, re0, 0), VAL_E0);
		re1 = cabi_list_get_struct(rh, rlist, 1);
		assert_int_equal((uint32_t)cabi_get_u32(rh, re1, 0), VAL_E1);
		cabi_builder_free(rh);
	}

	/* our bytes -> c-capnproto decoder. */
	{
		struct capn c2;
		capn_ptr root2, rs2, lst2, el0, el1;
		capn_text def, got;

		assert_int_equal(capn_init_mem(&c2, fbuf, (size_t)fn, 0), 0);
		root2 = capn_root(&c2);
		rs2 = capn_getp(root2, 0, 1);
		assert_int_equal(rs2.type, CAPN_STRUCT);
		assert_int_equal(capn_read32(rs2, 0), VAL_ROOT);

		memset(&def, 0, sizeof def);
		got = capn_get_text(rs2, 0, def);
		assert_int_equal(got.len, (int)strlen(NAME));
		assert_memory_equal(got.str, NAME, strlen(NAME));

		lst2 = capn_getp(rs2, 1, 1);
		assert_int_equal(lst2.len, 2);
		el0 = capn_getp(lst2, 0, 1);
		assert_int_equal(capn_read32(el0, 0), VAL_E0);
		el1 = capn_getp(lst2, 1, 1);
		assert_int_equal(capn_read32(el1, 0), VAL_E1);
		capn_free(&c2);
	}
}

/* The packed encoding worked example from
 * https://capnproto.org/encoding.html#packing : two words in, eight bytes out.
 * Validates the reference deflate against the spec vector so the golden bytes
 * above rest on a trusted encoder. */
static void test_packed_vector(void **state)
{
	const uint8_t unpacked[16] = {
	    0x08, 0x00, 0x00, 0x00, 0x03, 0x00, 0x02, 0x00,
	    0x19, 0x00, 0x00, 0x00, 0xaa, 0x01, 0x00, 0x00,
	};
	const uint8_t expected[8] = {0x51, 0x08, 0x03, 0x02, 0x31, 0x19, 0xaa, 0x01};
	uint8_t out[64];
	struct capn_stream s;
	int rc;
	size_t produced;
	(void)state;

	memset(&s, 0, sizeof s);
	s.next_in = unpacked;
	s.avail_in = sizeof unpacked;
	s.next_out = out;
	s.avail_out = sizeof out;

	rc = capn_deflate(&s);
	assert_int_equal(rc, 0);
	produced = sizeof out - s.avail_out;
	assert_int_equal(produced, sizeof expected);
	assert_memory_equal(out, expected, sizeof expected);
}

/* Packed golden: our packed serialization must equal the reference deflate
 * (validated against the spec vector above) applied to the shared golden
 * flat bytes; and the reference's packed output must round-trip through our
 * packed decoder. */
static void test_packed_golden(void **state)
{
	uint8_t fflat[512], fpacked[512], cpacked[512];
	int64_t fn, pn;
	struct capn_stream s;
	size_t cn_packed;
	int h, root;
	(void)state;

	fn = build_cabi(fflat, sizeof fflat);
	assert_true(fn > 0);

	memset(&s, 0, sizeof s);
	s.next_in = fflat;
	s.avail_in = (size_t)fn;
	s.next_out = cpacked;
	s.avail_out = sizeof cpacked;
	assert_int_equal(capn_deflate(&s), 0);
	cn_packed = sizeof cpacked - s.avail_out;

	{
		int bh = cabi_builder_new();
		int rs, lst, e0, e1;
		int64_t w = 0;

		assert_true(bh >= 1);
		rs = cabi_new_struct(bh, 1, 2);
		cabi_set_u32(bh, rs, 0, (int32_t)VAL_ROOT);
		cabi_set_text(bh, rs, 0, NAME);
		lst = cabi_new_composite_list(bh, 2, 1, 1);
		e0 = cabi_list_get_struct(bh, lst, 0);
		cabi_set_u32(bh, e0, 0, (int32_t)VAL_E0);
		e1 = cabi_list_get_struct(bh, lst, 1);
		cabi_set_u32(bh, e1, 0, (int32_t)VAL_E1);
		cabi_setp(bh, rs, 1, lst);
		cabi_set_root(bh, rs);
		assert_int_equal(cabi_serialize_packed(bh, fpacked, sizeof fpacked, &pn), 0);
		cabi_builder_free(bh);
	}

	assert_int_equal((size_t)pn, cn_packed);
	assert_memory_equal(fpacked, cpacked, cn_packed);

	h = cabi_deserialize_packed(cpacked, (int64_t)cn_packed);
	assert_true(h >= 1);
	root = cabi_root(h);
	assert_int_equal((uint32_t)cabi_get_u32(h, root, 0), VAL_ROOT);
	cabi_builder_free(h);
}

/* Primitive List(Int32) golden: capn_new_list32 on the reference side,
 * cabi_new_list with the FOUR element-size code (4) on ours. */
static void test_primitive_list_golden(void **state)
{
	uint8_t fbuf[256], cbuf[256];
	int64_t fn = 0, cn;
	int i;
	(void)state;

	{
		int h = cabi_builder_new();
		int rs, lst;

		assert_true(h >= 1);
		rs = cabi_new_struct(h, 0, 1);
		lst = cabi_new_list(h, 4, 3); /* CAPNP_SZ_FOUR, 3 elements */
		for (i = 0; i < 3; i++)
			cabi_list_set_i32(h, lst, i, 10 * (i + 1));
		cabi_setp(h, rs, 0, lst);
		cabi_set_root(h, rs);
		assert_int_equal(cabi_serialize(h, fbuf, sizeof fbuf, &fn), 0);
		cabi_builder_free(h);
	}

	{
		struct capn c;
		capn_ptr root, rs;
		capn_list32 lst;

		capn_init_malloc(&c);
		root = capn_root(&c);
		rs = capn_new_struct(root.seg, 0, 1);
		lst = capn_new_list32(root.seg, 3);
		for (i = 0; i < 3; i++)
			capn_set32(lst, i, (uint32_t)(10 * (i + 1)));
		capn_setp(rs, 0, lst.p);
		capn_setp(root, 0, rs);
		cn = capn_write_mem(&c, cbuf, sizeof cbuf, 0);
		capn_free(&c);
	}

	assert_true(fn > 0);
	assert_int_equal(fn, cn);
	assert_memory_equal(fbuf, cbuf, (size_t)cn);
}

/* Canonical form of the golden message: single segment, no table, preorder,
 * with the composite list's null pointer section trimmed uniformly. Layout:
 * root pointer word + root struct (1+2) + "hi" text (1) + list tag word +
 * 2 x 1-word elements = 8 words. */
static void test_canonical_form(void **state)
{
	uint8_t fflat[512], canon[512];
	const uint8_t root_ptr[8] = {0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00};
	int64_t fn, wn = 0;
	int h;
	(void)state;

	fn = build_cabi(fflat, sizeof fflat);
	assert_true(fn > 0);
	h = cabi_deserialize(fflat, fn);
	assert_true(h >= 1);
	assert_int_equal(cabi_canonicalize(h, canon, sizeof canon, &wn), 0);
	cabi_builder_free(h);

	assert_int_equal(wn, 64);
	assert_memory_equal(canon, root_ptr, sizeof root_ptr);
}

int main(void)
{
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_golden_bytes),
	    cmocka_unit_test(test_cross_decode),
	    cmocka_unit_test(test_packed_vector),
	    cmocka_unit_test(test_packed_golden),
	    cmocka_unit_test(test_primitive_list_golden),
	    cmocka_unit_test(test_canonical_form),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
