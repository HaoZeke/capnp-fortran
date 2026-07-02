@0xd3e9a5f17c842b09;

using Common = import "common.capnp";

const answer :Int32 = 42;
const tau :Float64 = 6.283185307179586;
const greeting :Text = "hey there";

struct Sink {
  flag @0 :Bool = true;
  count @1 :Int32 = -7;
  ratio @2 :Float64 = 2.5;
  label @3 :Text = "unnamed";
  payload @4 :Data = 0x"de ad be ef";
  origin @5 :Common.Vec3 = (x = 1.5, y = 2.5, z = -3.5);
  state @6 :Common.Status = busy;
  tags @7 :List(Text);
  blobs @8 :List(Data);
  grid @9 :List(List(Int32));
  stuff @10 :AnyPointer;
  spots @11 :List(Common.Vec3);
  scores @12 :List(Int64) = [3, 1, 4];
}
