
proc InputString uses bx di, buf:WORD
    mov ah, 0ah
    mov dx, [buf]
    int 21h

    mov di, [buf]
    movzx bx, byte [di + 1]
    mov [di + bx + 2], byte 0

    ret
endp

proc PrintString pStr:WORD
    mov ah, 09h
    mov dx, [pStr]
    int 21h
    ret
endp

proc InputDouble uses si di bx, strBuffer:WORD, numBuffer:WORD
    mov si, [strBuffer]
    mov di, [numBuffer]

    stdcall InputString, si
    mov cx, [si + 1]
    lea bx, [si + 2]
    stdcall StringToDouble, bx, cx, di

    ret
endp

proc InputConstantExpression uses si, strBuffer:WORD, RPNBuffer:WORD, resultBuffer:WORD
    mov si, [strBuffer]

    stdcall InputString, si
    add si, 2
    stdcall ToRPN, si, [RPNBuffer], 0
    jnc .Return

    fclex
    stdcall CalcRPN, [RPNBuffer], [resultBuffer], 0
    jnc .Return

    fstsw ax
    test ax, 1
    stc
    jz .Return
    clc

    .Return:
    ret
endp


proc InputConstantExpressionUntilCorrect strBuffer:WORD, RPNBuffer:WORD, resultBuffer:WORD


     ret
endp
