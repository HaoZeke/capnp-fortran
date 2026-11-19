JSON union keys are applied in document order. An unknown discriminator
does not select an arm, but a plain member key in the same object still
does. A repeated known tag is rejected, and a void arm accepts any
payload.
