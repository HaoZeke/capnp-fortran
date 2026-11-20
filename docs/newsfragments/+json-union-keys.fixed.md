JSON union keys are applied in document order. An unknown discriminator
does not select an arm, but a plain member key in the same object still
does. A repeated known tag is rejected, and a void value accepts any
payload. A flattened struct is reused for later keys in the same
object, and a nested flattened union keeps the tag it already saw.
