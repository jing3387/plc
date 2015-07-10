import java.io.FileInputStream;
import java.nio.channels.FileChannel;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;

/* Java implementation of a unified-stack abstract machine

   In a real stack machine, the stack is an array (not a list as in the SML or
   F# model), and there is a special register called the stack pointer sp which
   is updated as the stack grows and shrinks. Accessing a variable x stored n
   deep into the stack can be done in constant time by address arithmetic
   relative to the stack pointer: just access the element stack[sp-n].

   The interpreter seval below is a simple bytecode machine: each instruction
   is a single integer (representable in a byte). Instructions with arguments,
   such as SCST and SVAR, simply take their arguments from the next integer in
   the instruction stream.

   This is a Java program but might be written in C instead; it does not rely
   on object-orientation or garbage collection. */

class Machine {
    final static int
        SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6;

    public static void main(String[] args) {
        ArrayList<Integer> instrs = new ArrayList<Integer>();
        try (FileInputStream stream = new FileInputStream(args[0])) {
            FileChannel inChannel = stream.getChannel();
            ByteBuffer buffer = inChannel.map(
                FileChannel.MapMode.READ_ONLY,
                0,
                inChannel.size()
            );
            IntBuffer intBuffer = buffer.asIntBuffer();
            while (intBuffer.hasRemaining())
                instrs.add(intBuffer.get());
            Integer[] instrsArr = instrs.toArray(new Integer[instrs.size()]);
            System.out.println(seval(instrsArr));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static int seval(Integer[] code) {
        int[] stack = new int[1000]; // Evaluation and env stack.
        int sp = -1;                 // Pointer to current stack top.
        int pc = 0;                  // Program counter.
        int instr;                   // Current instruction.

        while (pc < code.length) {
            switch (instr = code[pc++]) {
            case SCST:
                stack[sp+1] = code[pc++];
                sp++;
                break;
            case SVAR:
                stack[sp+1] = stack[sp-code[pc++]];
                sp++;
                break;
            case SADD:
                stack[sp-1] = stack[sp-1] + stack[sp];
                sp--;
                break;
            case SSUB:
                stack[sp-1] = stack[sp-1] - stack[sp];
                sp--;
                break;
            case SMUL:
                stack[sp-1] = stack[sp-1] * stack[sp];
                sp--;
                break;
            case SPOP:
                sp--;
                break;
            case SSWAP:
                int tmp = stack[sp];
                stack[sp] = stack[sp-1];
                stack[sp-1] = tmp;
                break;
            default:
                throw new RuntimeException(
                    "Illegal instruction " + instr + " at address " + (pc-1)
                );
            }
        }
        return stack[sp];
    }
}
