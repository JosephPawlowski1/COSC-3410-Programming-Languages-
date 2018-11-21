C            PROGRAM ASSIGNMENT#4
C 	Written by Joseph Pawlowski Oct 2018
			
		
			REAl G(22,22)
			INTEGER MOLCOUNT 
			X = RAND(TIME())
			Y = RAND(TIME())
			
			DO 2 J=1,5
			COUNT=0 
			CALL INIT(G,22)
 			DO 1 I=1,100
			CALL MOLPUT(G,X,Y)
1 			CONTINUE 
			CALL SINGLE(G,22,COUNT)
			CALL INIT(G,22)
2			CONTINUE
			END
C This subroutine intiolise the 2D array with all zeros 
		SUBROUTINE INIT(A,N)
		REAL A(N,N)
      
		DO 3 I=1,N
		DO 3 J=1,N
			A(I,J)=0
3   	 CONTINUE
			RETURN
			END
C This subroutine puts a molecule in a random spot in the 2D array   
		SUBROUTINE MOLPUT(A,X,Y)
			REAL A(22,22)
			INTEGER XCORD
			INTEGER YCORD
			
4			X = RAND(0)
			Y = RAND(0)
			XCORD = 19*X+3.0
			YCORD = 19*Y+3.0
			
			IF(A(XCORD,YCORD) == 1) GOTO 4
				A(XCORD,YCORD) = 1.0
				
				
				CONTINUE 
				RETURN
				END
C Thsi Subroutine finds all of the isolated molecules and displays the number 			
		SUBROUTINE SINGLE(A,N,COUNT)
			REAL A(N,N)
			INTEGER COUNT = 0
			 
			
			DO 5 I=2, N-1
			DO 5 J=2, N-1
			
			IF( (A(I,J) == 0)) GOTO 5
			IF (A(I+1,J) == 0 .AND. A(I-1, J) == 0 .AND. A(I,J+1) == 0
     &      .AND. A(I,J-1) == 0) COUNT = COUNT + 1
			A(I,J) = 2.0
5			CONTINUE
			PRINT *, "Isolated molecules:  " , COUNT 
			RETURN 
			END
			
C OutPut:	Isolated molecules:     25.0000000
C 			Isolated molecules:     24.0000000
C 			Isolated molecules:     38.0000000
C			Isolated molecules:     24.0000000
C			Isolated molecules:     33.0000000