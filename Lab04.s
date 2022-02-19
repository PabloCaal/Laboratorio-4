
    ; Archivo:	    Prelab04.s
    ; Proyecto:	    Laboratorio_04 (Interrupciones)
    ; Dispositivo:  PIC16F887
    ; Autor:	    Pablo Caal
    ; Compilador:   pic-as (v2.30), MPLABX V5.40
    ;
    ; Programa:	Contador de 4-bits empleando interrupciones
    ; Hardware:	LED's en el puerto
    ;		Push botons en el puerto B
    ;		Display de 7 segmentos en puerto C y D
    ;
    ; Creado: 15 feb, 2022
    ; Última modificación: 18 feb, 2022
    
    PROCESSOR 16F887
    #include <xc.inc>

    ; CONFIG1
	CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN)
	CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
	CONFIG  PWRTE = OFF            ; Power-up Timer Enable bit (PWRT enabled)
	CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
	CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
	CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)

	CONFIG  BOREN = OFF           ; Brown Out Reset Selection bits (BOR disabled)
	CONFIG  IESO = OFF            ; Internal External Switchover bit (Internal/External Switchover mode is disabled)
	CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is disabled)
	CONFIG  LVP = OFF              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

    ; CONFIG2
	CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
	CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)

    ; Definición de valores constantes (Correspondiente a los botones del puerto B)
	B_INC	    EQU 0	; Valor constante equivalente
	B_DEC	    EQU 1	; Valor constante equivalente 
	
    ; Status de las interrupciones
    PSECT udata_shr		; Memoria compartida
	W_TEMP:		DS 1	; 1 byte
	STATUS_TEMP:	DS 1	; 1 byte
    
    ; Variables globales
	PSECT udata_bank0		; common memory
	CONT0:		DS 1		; Variable del Contador con botones
	CONT1:		DS 1		; Variable del Contador de segundos (DISPLAY1)
	CONT2:		DS 1		; Variable del Contador de decenas (DISPLAY2)
      
    ;-------------------------- VECTOR RESET -----------------------------------
    PSECT resVect, class=CODE, abs, delta=2
    ORG 00h			; Posición 0000h para el reset
    resetVec:
        PAGESEL main		; Cambio de pagina
        GOTO    main

    ;-------------------- SUBRUTINAS DE INTERRUPCION ---------------------------
    ORG 04h			; Posición 0004h para las interrupciones
    PUSH:			
	MOVWF   W_TEMP		; Guardamos en W el valor de la variable W_TEMP
	SWAPF   STATUS, W	; Hacemos swap de nibbles y guardamos en W
	MOVWF   STATUS_TEMP	; Almacenamos W en variable STATUS_TEMP
	
    ISR:			; Verificación de banderas de las interrupciones
	BTFSC   RBIF		; Vericamos si hay interrupción de cambio en el puerto B
	CALL    INT_B		; Subrutina INT_B
	
	BTFSC	T0IF		; Verificamos si hay interrupción del TIMER0
	CALL	INT_TMR0	; Subrutina INT_TMR0
	
    POP:				
	SWAPF   STATUS_TEMP, W	; Hacemos swap de nibbles y guardamos en W
	MOVWF   STATUS		; Trasladamos W al registro STATUS
	SWAPF   W_TEMP, F	; Hacemos swap de nibbles y guardamos en W_TEMP 
	SWAPF   W_TEMP, W	; Hacemos swap de nibbles y guardamos en W
	RETFIE
    
    ;------------------------ RUTINAS PRINCIPALES ------------------------------
    PSECT code, delta=2, abs
    ORG 100h	; posición 100h para el codigo
    
    main:
	CALL	CONFIG_IO	    ; Comfiguración de los puertos	
	CALL	CONFIG_CLK	    ; Configuración del osciloscopio
	CALL	CONIFG_INTERRUPT    ; Configuracion de INTERRUPCIONES
	CALL	CONFIG_IOCRB	    ; Configuración de 
	CALL	CONFIG_TIMER0
	
	CLRF	CONT1		; Reseteo de la variable global CONT1
	CLRF	CONT2		; Reseteo de la variable global CONT2
	BANKSEL PORTA
	
    loop:			; Rutina que se estará ejecutando indefinidamente 
	MOVF    CONT1, W	; Valor de contador 1 a W para buscarlo en la tabla hexadecimal
	CALL    TABLA		; Buscamos caracter de CONT1 en la tabla hexadecimal
	MOVWF   PORTC		; Guardamos caracter de CONT1 en PORTC
	
	MOVF    CONT2, W	; Valor de contador 2 a W para buscarlo en la tabla hexadecimal
	CALL    TABLA		; Buscamos caracter de CONT2 en la tabla hexadecimal
	MOVWF   PORTD		; Guardamos caracter de CONT2 en PORTD
	
	CALL	VERIFICAR_DECENA    ; Llamar subrutina de verificación de decena
	CALL	VERIFICAR_MINUTO    ; Llamar subrutina de verificación de minuto
	
	GOTO	loop		; Volvemos a comenzar con el loop
	
    ;--------------------------- SUBRUTINAS VARIAS -----------------------------
    INT_B:
	BANKSEL PORTB
	BTFSS   PORTB, B_INC	; Verificar si el bit 0 del puerto B está presionado
	INCF    PORTA		; Incrementar contador 
	BTFSS   PORTB, B_DEC	; Verificar si el bit 1 del puerto B no está presionado
	DECF    PORTA		; Decrementar contador
	BCF	RBIF		; Limpiar la bandera de cambio del PORTB
	return  
	
    INT_TMR0:
	CALL	RESET_TIMER
	INCF	CONT0		; Incrementamos al contador cada vez que se eleve la bandera (20ms)
    	MOVF	CONT0, W	; Colocamos el valor del CONT0 en la variable W
	SUBLW   50		; Restamos 50 al W (Condicional)
	BTFSS   ZERO		; Verificación de la bandera del ZERO
	return			
	INCF	CONT1		; Incrementar el CONT1
	CLRF	CONT0		; Resetear la variable contador
	return
	
    RESET_TIMER:
	BANKSEL TMR0		; Redireccionamos de banco
	MOVLW   100		; Valor de N de la ecuación
	MOVWF   TMR0		; Cálculo de retardo (20 ms)
	BCF	T0IF		; Limpiamos bandera de interrupción
	return

    VERIFICAR_MINUTO:
	MOVF	CONT2, W    ; Colocar el valor del contador de segundos en W
	SUBLW	6	    ; Restar 6 al valor del contador de decenas
	BTFSS	ZERO	    ; Verificación de la bandera del ZERO
	return
	CLRF	CONT1	    ; Reinicio del contador 1
	CLRF	CONT2	    ; Reinicio del contador 2
	return
	
    VERIFICAR_DECENA:
	MOVF	CONT1, W    ; Colocar el valor del contador de segundos en W
	SUBLW	10	    ; Restar 10 al valor del contador de segundos
	BTFSS   ZERO	    ; Verificación de la bandera del ZERO
	return
	INCF	CONT2
	CLRF	CONT1
	return
		
    ;--------------------- SUBRUTINAS DE CONFIGURACIÓN -------------------------
    CONFIG_TIMER0:
	BANKSEL OPTION_REG	; Redireccionamos de banco
	BCF	T0CS		; Configuramos al timer0 como temporizador
	BCF	PSA		; Configurar el Prescaler para el timer0 (No para el Wathcdog timer)
	BSF	PS2
	BSF	PS1
	BCF	PS0		; PS<2:0> -> 110 (Prescaler 1:128)
	CALL	RESET_TIMER	; Reiniciamos la bandera interrupción
	return
    
    CONFIG_CLK:			; Rutina de configuración de oscilador
	BANKSEL OSCCON	    
	BSF	OSCCON, 0
	BCF	OSCCON, 4
	BSF	OSCCON, 5
	BSF	OSCCON, 6	; Oscilador con reloj de 4 MHz
	return

    CONFIG_IOCRB:
	BANKSEL TRISB
	BSF	IOCB, B_INC	; Habilitar el registro IOCB para el primer bit
	BSF	IOCB, B_DEC	; Habilitar el registro IOCB para el segundo bit
	
	BANKSEL PORTB
	MOVF    PORTB, W	; Mover el valor del puerto B al registro W
	BCF	RBIF		; Limpieza de la bandera de interrupción por cambio RBIF
	return

    CONIFG_INTERRUPT:
	BANKSEL INTCON
	BSF	GIE		; Habilitamos a todas las interrupciones
	BSF	RBIE		; Habilitamos las interrupciones por cambio de estado del PORTB
	BCF	RBIF		; Limpieza de la bandera de la interrupción de cambio
	BSF	T0IE		; Habilitamos la interrupción del TMR0
	BCF	T0IF		; Limpieza de la bandera de TMR0
	return
	
    CONFIG_IO:
	BANKSEL ANSEL		; Direccionamos de banco
	CLRF    ANSEL
	CLRF    ANSELH		; Configurar como digitales
	
	BANKSEL TRISA		; Direccionamos de banco
	BSF	TRISB, 0	; Habilitamos como entrada al bit 0 de PORTB
	BSF	TRISB, 1	; Habilitamos como entrada al bit 1 de PORTB 
	BCF	TRISB, 2
	BCF	TRISB, 3
	BCF	TRISB, 4
	BCF	TRISB, 5
	BCF	TRISB, 6
	BCF	TRISB, 7
	BCF	TRISA, 0	; Habilitamos como salidas los 4 bsf del PORTA
	BCF	TRISA, 1
	BCF	TRISA, 2
	BCF	TRISA, 3
	BCF	TRISC, 0	; Habilitamos como salidas los 7 bsf del PORTC
	BCF	TRISC, 1
	BCF	TRISC, 2
	BCF	TRISC, 3
	BCF	TRISC, 4
	BCF	TRISC, 5
	BCF	TRISC, 6
	BCF	TRISD, 0	; Habilitamos como salidas los 7 bsf del PORTD
	BCF	TRISD, 1
	BCF	TRISD, 2
	BCF	TRISD, 3
	BCF	TRISD, 4
	BCF	TRISD, 5
	BCF	TRISD, 6
	
	BCF	OPTION_REG, 7   ; Habilitar las resistencias pull-up (RPBU)
	BSF	WPUB, B_INC	; Habilita el registro de pull-up en RB0 
	BSF	WPUB, B_DEC	; Habilita el registro de pull-up en RB1

	BANKSEL PORTA		; Direccionar de banco
	CLRF    PORTA		; Limpieza de PORTA
	CLRF    PORTB		; Limpieza de PORTB
	CLRF	PORTC
	CLRF	PORTD
	return
	
    ;------------------------ TABLA  HEXADECIMAL -------------------------------
    ORG 200h
    TABLA:
	CLRF    PCLATH		; Limpiamos registro PCLATH
	BSF	PCLATH, 1	; Posicionamos el PC en dirección 02xxh
	ANDLW   0x0F		; no saltar más del tamaño de la tabla
	ADDWF   PCL		; Apuntamos el PC a caracter en ASCII de CONT
	RETLW   00000001B	; 0
	RETLW   01001111B	; 1
	RETLW   00010010B	; 2
	RETLW   00000110B	; 3
	RETLW   01001100B	; 4
	RETLW   00100100B	; 5
	RETLW   00100000B	; 6
	RETLW   00001111B	; 7
	RETLW   00000000B	; 8
	RETLW   00000100B	; 9
	RETLW   00001000B	; 10 (A)
	RETLW   01100000B	; 11 (B)
	RETLW   00110001B	; 12 (C)
	RETLW   01000010B	; 13 (D)
	RETLW   00110000B	; 14 (E)
	RETLW   00111000B	; 15 (F)
    END