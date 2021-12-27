package edu.colorado.csci3155.project1

import scala.:+


sealed trait StackMachineInstruction
case class LoadIns(s: String) extends StackMachineInstruction
case class  StoreIns(s: String) extends StackMachineInstruction
case class PushIns(f: Double) extends StackMachineInstruction
case object AddIns extends StackMachineInstruction
case object SubIns extends StackMachineInstruction
case object MultIns extends StackMachineInstruction
case object DivIns extends StackMachineInstruction
case object ExpIns extends StackMachineInstruction
case object LogIns extends StackMachineInstruction
case object SinIns extends StackMachineInstruction
case object CosIns extends StackMachineInstruction
case object PopIns extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Environment.t,
                                 ins: StackMachineInstruction): (List[Double], Environment.t) = {
        ins match {
            case LoadIns(identifier) =>
                val popped :: listContinued = stack
                (listContinued, env.filter { case (a, _) => a != identifier } :+ (identifier, popped))
            case StoreIns(identifier) =>
                val idList = List(env.filter { case (a, _) => a == identifier }.head._2)
                if (idList.isEmpty) {
                    throw new IllegalArgumentException("Identifier Not Present")
                } else {
                    (idList ++ stack, env)
                }
            case PushIns(toPush) =>
                (List(toPush) ++ stack, env)
            case AddIns =>
                val num1 :: num2 :: listContinued = stack
                if (num1 == Nil || num2 == Nil) {
                    throw new IllegalArgumentException("Missing Argument")
                } else {
                    (List(num1 + num2) ++ listContinued, env)
                }
            case SubIns =>
                val num1 :: num2 :: listContinued = stack
                if (num1 == Nil || num2 == Nil) {
                    throw new IllegalArgumentException("Missing Argument")
                } else {
                    (List(num2 - num1) ++ listContinued, env)
                }
            case MultIns =>
                val num1 :: num2 :: listContinued = stack
                if (num1 == Nil || num2 == Nil) {
                    throw new IllegalArgumentException("Missing Argument")
                } else {
                    (List(num1 * num2) ++ listContinued, env)
                }
            case DivIns =>
                val num1 :: num2 :: listContinued = stack
                if (num1 == Nil || num2 == Nil) {
                    throw new IllegalArgumentException("Missing Argument")
                } else if (num1 == 0) {
                    throw new IllegalArgumentException("Divide By 0 Error")
                } else {
                    (List(num2/num1) ++ listContinued, env)
                }
            case ExpIns =>
                val num1 :: listContinued = stack
                if (num1 == Nil){
                    throw new IllegalArgumentException("Missing Argument")
                } else {
                    (List(math.exp(num1)) ++ listContinued, env)
                }
            case LogIns =>
                val num1 :: listContinued = stack
                if (num1 == Nil){
                    throw new IllegalArgumentException("Missing Argument")
                } else if (num1 < 0){
                    throw new IllegalArgumentException("Negative Log Error")
                } else {
                    (List(math.log(num1)) ++ listContinued, env)
                }
            case SinIns =>
                val num1 :: listContinued = stack
                if (num1 == Nil){
                    throw new IllegalArgumentException("Missing Argument")
                } else {
                    (List(math.sin(num1)) ++ listContinued, env)
                }
            case CosIns =>
                val num1 :: listContinued = stack
                if (num1 == Nil){
                    throw new IllegalArgumentException("Missing Argument")
                } else {
                    (List(math.cos(num1)) ++ listContinued, env)
                }
            case PopIns =>
                val popped :: listContinued = stack
                if (popped == Nil) {
                    throw new IllegalArgumentException("Stack Empty")
                } else {
                    (listContinued, env)
                }
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Environment.t =
        {
            instructionList.foldLeft(List[Double](), Environment.empty) { case((stack,environment), instruction) => emulateSingleInstruction(stack, environment, instruction)}._2
        }
}