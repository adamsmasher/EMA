module MIPSConst where

supportedInstructions = words "add addi addiu addu and andi beq bgez bgezal bgtz blez bltz bltzal bne div divu j jal jalr jr lb lbu lh lhu lui lw lwl lwr mfhi mflo mthi mtlo mult multu nor or ori sb sh sll sllv slt slti sltiu sltu sra srav srl srlv sub subu sw swl swr xor xori"

special = 0x00 :: Integer
regimm  = 0x01 :: Integer
jOp     = 0x02 :: Integer
jalOp   = 0x03 :: Integer
beqOp   = 0x04 :: Integer
bneOp   = 0x05 :: Integer
blezOp  = 0x06 :: Integer
bgtzOp  = 0x07 :: Integer
addiOp  = 0x08 :: Integer
addiuOp = 0x09 :: Integer
sltiOp  = 0x0A :: Integer
sltiuOp = 0x0B :: Integer
andiOp  = 0x0C :: Integer
oriOp   = 0x0D :: Integer
xoriOp  = 0x0E :: Integer
luiOp   = 0x0F :: Integer
lbOp    = 0x20 :: Integer
lhOp    = 0x21 :: Integer
lwlOp   = 0x22 :: Integer
lwOp    = 0x23 :: Integer
lbuOp   = 0x24 :: Integer
lhuOp   = 0x25 :: Integer
lwrOp   = 0x26 :: Integer
sbOp    = 0x28 :: Integer
shOp    = 0x29 :: Integer
swlOp   = 0x2A :: Integer
swOp    = 0x2B :: Integer
swrOp   = 0x2E :: Integer

sllFunc     = 0x00 :: Integer 
bgezFunc    = 0x01 :: Integer
srlFunc     = 0x02 :: Integer
sraFunc     = 0x03 :: Integer
sllvFunc    = 0x04 :: Integer
jalrFunc    = 0x05 :: Integer
srlvFunc    = 0x06 :: Integer
sravFunc    = 0x07 :: Integer
jrFunc      = 0x08 :: Integer
syscallFunc = 0x0C :: Integer
breakFunc   = 0x0D :: Integer
mfhiFunc    = 0x10 :: Integer
mthiFunc    = 0x11 :: Integer
mfloFunc    = 0x12 :: Integer
mtloFunc    = 0x13 :: Integer
multFunc    = 0x18 :: Integer
multuFunc   = 0x19 :: Integer
divFunc     = 0x1A :: Integer
divuFunc    = 0x1B :: Integer
addFunc     = 0x20 :: Integer
adduFunc    = 0x21 :: Integer
subFunc     = 0x22 :: Integer
subuFunc    = 0x23 :: Integer
andFunc     = 0x24 :: Integer
orFunc      = 0x25 :: Integer
xorFunc     = 0x26 :: Integer
norFunc     = 0x27 :: Integer
sltFunc     = 0x2A :: Integer
sltuFunc    = 0x2B :: Integer

bltzFunc    = 0x00 :: Integer
bltzalFunc  = 0x10 :: Integer
bgezalFunc  = 0x11 :: Integer

