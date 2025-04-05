include 'macro/proc16.inc'
include 'ConvertationUtils.inc'

NUMBER_TOKEN_TYPE = 1
OPERATION_TOKEN_TYPE = 2
VARIABLE_TOKEN_TYPE = 3

SIN_FUNCTION_OPCODE = 1
SIN_FUNCTION_NAME db "sin"

COS_FUNCTION_OPCODE = 2
COS_FUNCTION_NAME db "cos"

TAN_FUNCTION_OPCODE = 3
TAN_FUNCTION_NAME db "tan"
TAN_FUNCTION_ALIAS db "tg"

COT_FUNCTION_OPCODE = 4
COT_FUNCTION_NAME db "cot"
COT_FUNCTION_ALIAS db "ctg"

SQRT_FUNCTION_OPCODE = 5
SQRT_FUNCTION_NAME db "sqrt"

ABS_FUNCTION_OPCODE = 6
ABS_FUNCTION_NAME db "abs"

UNARY_MINUS_OPCODE = 7

ARCSIN_FUNCTION_OPCODE = 8
ARCSIN_FUNCTION_NAME db "arcsin"

ARCCOS_FUNCTION_OPCODE = 9
ARCCOS_FUNCTION_NAME db "arccos"

ARCTAN_FUNCTION_OPCODE = 10
ARCTAN_FUNCTION_NAME db "arctan"
ARCTAN_FUNCTION_ALIAS db "arctg"

ARCCOT_FUNCTION_OPCODE = 11
ARCCOT_FUNCTION_NAME db "arccot"
ARCCOT_FUNCTION_ALIAS db "arcctg"

LN_FUNCTION_OPCODE = 12
LN_FUNCTION_NAME db "ln"


OP_PRIORITIES db "+", 1, "-", 1, UNARY_MINUS_OPCODE, 1, "*", 2, "/", 2, "^", 3, SIN_FUNCTION_OPCODE, 4, \
                 COS_FUNCTION_OPCODE, 4, TAN_FUNCTION_OPCODE, COT_FUNCTION_OPCODE, 4, 4, SQRT_FUNCTION_OPCODE, 4,\
                 ABS_FUNCTION_OPCODE, 4, ARCCOS_FUNCTION_OPCODE, 4, ARCSIN_FUNCTION_OPCODE, 4, ARCTAN_FUNCTION_OPCODE, 4, \
                 ARCCOT_FUNCTION_OPCODE, 4, LN_FUNCTION_OPCODE, 4, 0

e dq $4005BF0A8B145769
TWO dw 2


proc ToRPN uses si di bx, pSource:WORD, pDestBuf:WORD, variableLetter:BYTE
    local CurrentTokenStart dw ?
    local PrevTokenType dw ?

    push 0

    mov di, [pDestBuf]
    add di, 2
    mov [PrevTokenType], 0

    mov si, [pSource]
    cmp byte [si], 0
    je .ReturnError

    .ReadLoop:
        movzx ax, byte [si]

        cmp ax, 0
        je .ReturnSuccess

        ; Skip whitespace characters
        cmp al, ' '
        je ..NextIteration
        cmp al, 13
        je ..NextIteration
        cmp al, 10
        je ..NextIteration
        cmp al, 9
        je ..NextIteration

        ; If '(', just push to stack
        cmp al, '('
        jne .CheckCloseParen

        cmp [PrevTokenType], 0
        je @F
        cmp byte [PrevTokenType], OPERATION_TOKEN_TYPE
        je @F
        cmp byte [PrevTokenType], '('
        je @F
        jmp .ReturnError

        @@:
        push ax
        mov [PrevTokenType], '('
        jmp ..NextIteration

        .CheckCloseParen:
        ; If ')', pop from stack until '(' met
        cmp al, ')'
        jne .CheckMinus

        cmp [PrevTokenType], ')'
        je @F
        cmp byte [PrevTokenType], OPERATION_TOKEN_TYPE
        je @F
        cmp [PrevTokenType], VARIABLE_TOKEN_TYPE
        je @F
        cmp [PrevTokenType], NUMBER_TOKEN_TYPE
        je @F
        jmp .ReturnError

        @@:
        mov [PrevTokenType], ')'
        ..PopUntilOpenParenLoop:
            pop ax
            cmp ax, '('
            je ..NextIteration
            cmp ax, 0
            je .ReturnError

            mov [di], ah
            mov [di + 1], al
            add di, 2
            jmp ..PopUntilOpenParenLoop

        .CheckMinus:
        cmp ax, '-'
        jne @F
        mov dx, [PrevTokenType]

        cmp dx, NUMBER_TOKEN_TYPE
        je .BinaryOp
        cmp dx, VARIABLE_TOKEN_TYPE
        je .BinaryOp
        cmp dx, ')'
        je .BinaryOp

        mov [PrevTokenType], OPERATION_TOKEN_TYPE
        push OPERATION_TOKEN_TYPE * 256 + UNARY_MINUS_OPCODE
        jmp ..NextIteration

        @@:
        cmp ax, '+'
        je .BinaryOp
        cmp ax, '*'
        je .BinaryOp
        cmp ax, '/'
        je .BinaryOp
        cmp ax, '^'
        je .BinaryOp
        jmp @F

        .BinaryOp:
            mov [PrevTokenType], OPERATION_TOKEN_TYPE
            mov dx, ax
            mov dh, OPERATION_TOKEN_TYPE

            stdcall GetOpPriority, ax
            cmp ax, 0
            je .ReturnError
            mov cx, ax
            ..PopStackLoop:
                pop ax
                mov bx, ax
                cmp bh, OPERATION_TOKEN_TYPE
                jne ..PushToStack
                stdcall GetOpPriority, bx
                cmp ax, cx
                jb ..PushToStack
                mov [di], bh
                mov [di + 1], bl
                add di, 2
                jmp ..PopStackLoop

             ..PushToStack:
                 push bx
                 push dx
                 jmp ..NextIteration

        @@:
        ; If character is digit, read number
        stdcall IsDigit, ax
        jnc .CheckLetter

        ; Check previous token to be only operation or open parenthesis
        cmp [PrevTokenType], 0
        je @F
        cmp [PrevTokenType], '('
        je @F
        cmp [PrevTokenType], OPERATION_TOKEN_TYPE
        je @F
        jmp .ReturnError

        @@:
        mov [CurrentTokenStart], si
        xor ax, ax
        ..ReadNumberLoop:
            movzx ax, byte [si]

            stdcall IsDigit, ax
            jc ...NextIteration
            cmp al, '.'
            jne ..WriteNumberToken
            cmp ah, 0
            jne ..WriteNumberToken
            inc ah

            ...NextIteration:
                inc si
                jmp ..ReadNumberLoop

        ..WriteNumberToken:
            mov [PrevTokenType], NUMBER_TOKEN_TYPE

            mov [di], byte NUMBER_TOKEN_TYPE
            inc di

            mov cx, si
            sub cx, [CurrentTokenStart]
            stdcall StringToDouble, [CurrentTokenStart], cx, di
            add di, 8
            dec si
            jmp ..NextIteration

        .CheckLetter:
        stdcall IsLetter, ax
        jnc .ReturnError
        movzx dx, byte [si + 1]
        push ax
        stdcall IsLetter, dx
        pop ax
        jc .CheckPI

        or ax, 32

        cmp al, [variableLetter]
        jne .CheckE

        cmp [PrevTokenType], NUMBER_TOKEN_TYPE
        jne @F
        mov ax, '*'
        dec si
        jmp .BinaryOp

        @@:
        cmp [PrevTokenType], 0
        je @F
        cmp [PrevTokenType], OPERATION_TOKEN_TYPE
        je @F
        cmp [PrevTokenType], '('
        je @F
        jmp .ReturnError

        @@:
        mov [PrevTokenType], VARIABLE_TOKEN_TYPE
        mov [di], byte VARIABLE_TOKEN_TYPE
        inc di
        jmp ..NextIteration

        .CheckE:
        cmp ax, 'e'
        jne .ReturnError
        mov [PrevTokenType], NUMBER_TOKEN_TYPE
        mov [di], byte NUMBER_TOKEN_TYPE
        inc di
        fld [e]
        fstp qword [di]
        add di, 8
        jmp ..NextIteration

        .CheckPI:
        movzx dx, byte [si + 2]
        push ax
        stdcall IsLetter, dx
        pop ax
        jc ..ReadFunction
        cmp word [si], 'i' shl 8 + 'p'
        jne ..ReadFunction
        mov [PrevTokenType], NUMBER_TOKEN_TYPE
        mov [di], byte NUMBER_TOKEN_TYPE
        inc di
        fldpi
        fstp qword [di]
        add di, 8
        inc si
        jmp ..NextIteration

        ..ReadFunction:
        mov [CurrentTokenStart], si
        inc si
        ..ReadFunctionLoop:
            movzx ax, byte [si]
            stdcall IsAlphanumeric, ax
            jnc ..WriteFunctionToken
            inc si
            jmp ..ReadFunctionLoop

        ..WriteFunctionToken:
            mov cx, si
            sub cx, [CurrentTokenStart]
            stdcall GetFunctionOpcode, [CurrentTokenStart], cx
            cmp ax, 0
            je .ReturnError
            mov ah, OPERATION_TOKEN_TYPE
            push ax
            mov [PrevTokenType], OPERATION_TOKEN_TYPE
            dec si
            jmp ..NextIteration

        ..NextIteration:
            inc si
            jmp .ReadLoop

    .ReturnSuccess:
        mov bx, 1
        jmp .PopRest

    .ReturnError:
       xor bx, bx

    .PopRest:
        pop ax
        cmp ax, 0
        je .Return

        cmp ah, OPERATION_TOKEN_TYPE
        jne @F
        xchg ah, al

        @@:
        mov [di], ax
        add di, 2
        jmp .PopRest

    .Return:
        mov [di], byte 0
        mov ax, di
        sub ax, [pDestBuf]
        add ax, 3
        mov di, [pDestBuf]
        mov [di], ax

        clc
        test bx, bx
        jz @F
        stc

        @@:
        ret
endp


proc CalcRPN uses si di bx, pSrc:WORD, pResultBuf:WORD, pX:WORD
    local operand1 dq ?
    local operand2 dq ?
    local opResult dq ?

    mov si, [pSrc]
    add si, 2
    mov di, [pResultBuf]

    .CalcLoop:
        movzx ax, byte [si]
        cmp ax, 0
        je .ReturnSuccess

        cmp ax, NUMBER_TOKEN_TYPE
        jne @F

        inc si
        push dword [si + 4]
        push dword [si]
        add si, 7
        jmp .CalcLoop.NextIteration

        @@:
        cmp ax, VARIABLE_TOKEN_TYPE
        jne @F
        mov bx, [pX]
        push dword [bx + 4]
        push dword [bx]
        jmp .CalcLoop.NextIteration

        @@:
        cmp ax, OPERATION_TOKEN_TYPE
        jne .ReturnError
        inc si
        movzx ax, byte [si]

        pop dword [operand2]
        pop dword [operand2 + 4]
        fld [operand2]

        cmp ax, UNARY_MINUS_OPCODE
        jne @F
        fchs
        jmp .PushOpResult

        @@:
        cmp ax, SIN_FUNCTION_OPCODE
        jne @F
        fsin
        jmp .PushOpResult

        @@:
        cmp ax, COS_FUNCTION_OPCODE
        jne @F
        fcos
        jmp .PushOpResult

        @@:
        cmp ax, TAN_FUNCTION_OPCODE
        jne @F
        fptan
        fstp st0
        jmp .PushOpResult

        @@:
        cmp ax, COT_FUNCTION_OPCODE
        jne @F
        fptan
        fdivrp
        jmp .PushOpResult

        @@:
        cmp ax, SQRT_FUNCTION_OPCODE
        jne @F
        fsqrt
        jmp .PushOpResult

        @@:
        cmp ax, ABS_FUNCTION_OPCODE
        jne @F
        fabs
        jmp .PushOpResult

        @@:
        cmp ax, ARCCOS_FUNCTION_OPCODE
        jne @F
        fld st0
        fmul st0, st0
        fld1
        fsubrp
        fsqrt
        fxch
        fpatan
        jmp .PushOpResult

        @@:
        cmp ax, ARCSIN_FUNCTION_OPCODE
        jne @F
        fld st0
        fmul st0, st0
        fld1
        fsubrp
        fsqrt
        fpatan
        jmp .PushOpResult

        @@:
        cmp ax, ARCTAN_FUNCTION_OPCODE
        jne @F
        fld1
        fpatan
        jmp .PushOpResult

        @@:
        cmp ax, ARCCOT_FUNCTION_OPCODE
        jne @F
        fld1
        fpatan
        fldpi
        fidiv [TWO]
        fsubrp
        jmp .PushOpResult

        @@:
        cmp ax, LN_FUNCTION_OPCODE
        jne @F
        fldln2
        fxch
        fyl2x
        jmp .PushOpResult

        @@:
        pop dword [operand1]
        pop dword [operand1 + 4]
        fld [operand1]

        cmp ax, '+'
        jne @F
        faddp
        jmp .PushOpResult

        @@:
        cmp ax, '-'
        jne @F
        fsubrp
        jmp .PushOpResult

        @@:
        cmp ax, '*'
        jne @F
        fmulp
        jmp .PushOpResult

        @@:
        cmp ax, '/'
        jne @F
        fdivrp
        jmp .PushOpResult

        @@:
        cmp ax, '^'
        jne @F
        stdcall Power
        jmp .PushOpResult

        .PushOpResult:
            fstp [opResult]
            push dword [opResult + 4]
            push dword [opResult]

        .CalcLoop.NextIteration:
            inc si
            jmp .CalcLoop

    .ReturnError:
        clc
        jmp .Return

    .ReturnSuccess:
        mov bx, [pResultBuf]
        pop dword [bx]
        pop dword [bx + 4]
        stc

    .Return:
        ret
endp


; X^Y
; ST(0) = X, ST(1) = Y
proc Power
    local IntExp dw ?

    fld st1
    frndint
    fcomp st2
    fstsw ax
    sahf
    jne .RealPower

    fxch
    fistp [IntExp]
    mov ax, [IntExp]
    mov cx, ax
    cwd
    fld1
    fxch
    xor ax, dx
    sub ax, dx
    jnz .Entry

    fstp st1
    fstp st1
    jmp .Return

    .Loop:
    fmul st0, st0
    .Entry:
    shr ax, 1
    jnc .Loop
    fmul st1, st
    jnz .Loop
    fstp st0
    cmp cx, 0
    jge .Return

    fld1
    fdivrp
    jmp .Return

    .RealPower:
    fyl2x
    fld st0
    frndint
    fsub st1, st0
    fxch
    f2xm1
    fld1
    faddp
    fscale
    fstp st1

    .Return:
    ret
endp


proc GetFunctionOpcode funcStrBuf:WORD, strLen:WORD
    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], TAN_FUNCTION_ALIAS, 2
    jnc @F
    mov ax, TAN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], LN_FUNCTION_NAME, 2
    jnc @F
    mov ax, LN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], SIN_FUNCTION_NAME, 3
    jnc @F
    mov ax, SIN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], COS_FUNCTION_NAME, 3
    jnc @F
    mov ax, COS_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ABS_FUNCTION_NAME, 3
    jnc @F
    mov ax, ABS_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], TAN_FUNCTION_NAME, 3
    jnc @F
    mov ax, TAN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], COT_FUNCTION_NAME, 3
    jnc @F
    mov ax, COT_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], COT_FUNCTION_ALIAS, 3
    jnc @F
    mov ax, COT_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], SQRT_FUNCTION_NAME, 4
    jnc @F
    mov ax, SQRT_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ARCTAN_FUNCTION_ALIAS, 5
    jnc @F
    mov ax, ARCTAN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ARCSIN_FUNCTION_NAME, 6
    jnc @F
    mov ax, ARCSIN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ARCCOS_FUNCTION_NAME, 6
    jnc @F
    mov ax, ARCCOS_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ARCTAN_FUNCTION_NAME, 6
    jnc @F
    mov ax, ARCTAN_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ARCCOT_FUNCTION_NAME, 6
    jnc @F
    mov ax, ARCCOT_FUNCTION_OPCODE
    jmp .Return

    @@:
    stdcall StringsEqual, [funcStrBuf], [strLen], ARCCOT_FUNCTION_ALIAS, 6
    jnc @F
    mov ax, ARCCOT_FUNCTION_OPCODE
    jmp .Return

    @@:
    xor ax, ax

    .Return:
        ret
endp


proc GetOpPriority uses si, op:WORD
    mov si, OP_PRIORITIES
    mov ax, [op]
    .FindOpLoop:
        cmp [si], byte 0
        je .NotFound
        cmp [si], al
        jne .FindOpLoop.NextIteration

        movzx ax, byte [si + 1]
        jmp .Return

        .FindOpLoop.NextIteration:
            add si, 2
            jmp .FindOpLoop

    .NotFound:
        xor ax, ax

    .Return:
    ret
endp


