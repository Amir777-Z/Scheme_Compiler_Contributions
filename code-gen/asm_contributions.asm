;;; r8 : params
;;; r9 : | env |
extend_lexical_environment:
        mov rdi, r9
        inc rdi
        shl rdi, 3
        call malloc            ;; Allocate the ExtEnv - (1+|env|)
        mov rbx, rax           ;; store ExdEnv

        mov rcx, r9
        ;;; lea rsi, [rbp + 8 * 2] ;; Env on the stack
        mov rsi, ENV
        lea rdi, [rbx + 8]     ;; Allocated ExtEnv
        cld
        rep movsq              ;; Copy pointers of minor vectors

        mov rdi, r8            ;; count params
        shl rdi, 3
        call malloc            
        mov [rbx], rax         ;; rax = ExtEnv[0] = parameters vector

        mov rcx, r8
        lea rsi, [rbp + 8 * 4] ;; Start from Param0
        lea rdi, [rax]         ;; ExtEnv[0][0]
        cld
        rep movsq              ;; Copy parameters

        mov rax, rbx           ;; final allocated data should be in rax
        ret
		
		
L_code_ptr_bin_apply: ; We override the last frame with the apply frame, similarly to what we did with the tail-call
	enter 0, 0
        cmp COUNT, 2
        jl L_error_arg_count_2   
        mov rax, PARAM(0)               ; rax = proc
        assert_closure(rax)

        mov r8, COUNT
        push OLD_RBP
        push RET_ADDR
        push SOB_CLOSURE_ENV(rax)       ; push the function's env
        sub rsp, 8                      ; save space for the arguments count

        ; push the normal arguments from first to last (PARAM(1) = X_0 to PARAM(PARAMS_COUNT - 2) = X_n-1)
        mov rbx, r8
        sub rbx, 2

        lea rsi, [rbp + 8 * (4 + 1)]  ;;; PARAM(1)