


        SUBROUTINE printArray(inArr,sizeArr)
                INTEGER, INTENT(IN)     :: sizeArr
                COMPLEX, INTENT(IN)     :: inArr(sizeArr)

                
                write(*,*) inArr

        END SUBROUTINE



        PROGRAM misMatchDim
        IMPLICIT NONE

        INTEGER         :: theSizeArr
        COMPLEX         :: theArr(4,4,4)
        INTEGER         :: i,j,k,l=0


        theSizeArr=size(theArr)

        DO i=1,4
          DO j=1,4
            DO k=1,4
            theArr(i,j,k)=l
            l=l+1
            ENDDO
          ENDDO
        ENDDO  

        CALL printArray(theArr,theSizeArr)

        END PROGRAM misMatchDim
