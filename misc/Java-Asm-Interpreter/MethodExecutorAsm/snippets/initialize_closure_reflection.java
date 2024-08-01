
// Call Runtime.eval_string() method to evaluate a string and get an array of variable names (newEnv)
ctx.mv.visitMethodInsn(
    Opcodes.INVOKESTATIC, 
    "Runtime", 
    "eval_string", 
    "(LRuntime;Ljava/lang/String;)[Ljava/lang/String;", 
    false);

// Store the returned array of strings (newEnv) in a local variable
int newEnvIndex = ctx.mv.newLocal(Type.getType(String[].class));
ctx.mv.visitVarInsn(Opcodes.ASTORE, newEnvIndex);

// Create labels for the loop start and end
Label loopStart = new Label();
Label loopEnd = new Label();

// Create a local variable to use as the loop index (i)
int i = ctx.mv.newLocal(Type.INT_TYPE);

// Initialize the loop index to 2, as we skip the first two local variables (0 and 1)
ctx.mv.visitInsn(Opcodes.ICONST_2); // Push constant 2 onto the stack
ctx.mv.visitVarInsn(Opcodes.ISTORE, i); // Store the value into the local variable i

// Start of the loop
ctx.mv.visitLabel(loopStart);

// Load the current value of i and the length of the newEnv array to compare
ctx.mv.visitVarInsn(Opcodes.ILOAD, i); // Load the loop index (i)
ctx.mv.visitVarInsn(Opcodes.ALOAD, newEnvIndex); // Load the newEnv array
ctx.mv.visitInsn(Opcodes.ARRAYLENGTH); // Get the length of the newEnv array
ctx.mv.visitJumpInsn(Opcodes.IF_ICMPGE, loopEnd); // If i >= newEnv.length, jump to loopEnd

// Load the class name onto the stack
ctx.mv.visitLdcInsn(Type.getObjectType(evalCtx.javaClassName)); // Load the new class type

// Load the i-th element of the newEnv array (field name) onto the stack
ctx.mv.visitVarInsn(Opcodes.ALOAD, newEnvIndex); // Load the newEnv array
ctx.mv.visitVarInsn(Opcodes.ILOAD, i); // Load the loop index (i)
ctx.mv.visitInsn(Opcodes.AALOAD); // Get the element at index i from newEnv array

// Load the local variable to be copied (skip index 0 and 1)
ctx.mv.visitVarInsn(Opcodes.ALOAD, i); // Load the local variable at index i

// Reflectively fetch the Field object for the static field
ctx.mv.visitMethodInsn(
    Opcodes.INVOKEVIRTUAL, 
    "java/lang/Class", 
    "getDeclaredField", 
    "(Ljava/lang/String;)Ljava/lang/reflect/Field;", 
    false);

// Make the field accessible
ctx.mv.visitInsn(Opcodes.DUP); // Duplicate the Field object on the stack
ctx.mv.visitInsn(Opcodes.ICONST_1); // Push true onto the stack for setAccessible(true)
ctx.mv.visitMethodInsn(
    Opcodes.INVOKEVIRTUAL, 
    "java/lang/reflect/Field", 
    "setAccessible", 
    "(Z)V", 
    false);

// Set the static field with the local variable value
ctx.mv.visitInsn(Opcodes.SWAP); // Swap the Field object and the value on the stack
ctx.mv.visitInsn(Opcodes.ACONST_NULL); // Push null for the first argument of Field.set (static field)
ctx.mv.visitMethodInsn(
    Opcodes.INVOKEVIRTUAL, 
    "java/lang/reflect/Field", 
    "set", 
    "(Ljava/lang/Object;Ljava/lang/Object;)V", 
    false);

// Increment the loop index (i)
ctx.mv.visitIincInsn(i, 1); // Increment the loop index by 1

// Jump back to the start of the loop
ctx.mv.visitJumpInsn(Opcodes.GOTO, loopStart); // Jump to the start of the loop

// End of the loop
ctx.mv.visitLabel(loopEnd); // Mark the end of the loop
