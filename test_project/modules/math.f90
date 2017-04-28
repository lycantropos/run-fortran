module math

    use, intrinsic :: iso_fortran_env

    implicit none

    interface calculateStandartDeviation
            module procedure int_calculateSigma
            module procedure vector_calculateSigma
    end interface calculateStandartDeviation

    public :: PI, &
              NAN_64, &
              multiplyMatrixByVector, &
              calculateStandartDeviation

    real*8, parameter :: PI = 4.d0 * datan(1.0d0)
    ! TODO: add test as for different PCs NaN code can be different
    real*8, parameter :: NAN_64 = transfer(-2251799813685248_int64, 1._real64)


contains


    function multiplyMatrixByVector(A,x) result(y)

        real*8,dimension(3,3),intent(in) :: A
        real*8,dimension(3),intent(in) :: x
        real*8,dimension(3) :: y
        y(1) = A(1, 1)*x(1) + A(1, 2)*x(2) + A(1, 3)*x(3)
        y(2) = A(2, 1)*x(1) + A(2, 2)*x(2) + A(2, 3)*x(3)
        y(3) = A(3, 1)*x(1) + A(3, 2)*x(2) + A(3, 3)*x(3)

    end function

        
    subroutine int_calculateSigma(sum, &
                                  numberOfWDs, &
                                  velocityArray, &
                                  vel_avg, &
                                  sd)
        real*8, dimension(3), intent(in) :: sum
        integer, intent(in) :: numberOfWDs
        real*8, dimension(:, :), intent(in) :: velocityArray
        real*8, dimension(3), intent(out) :: vel_avg
        real*8, dimension(3), intent(out) :: sd
        integer :: i
        real*8, dimension(3) :: sumOfRestsSquared &
                                = (/0.d0, 0.d0, 0.d0/)

        vel_avg = sum / dfloat(numberOfWDs)

        do i = 1, numberOfWDs
            sumOfRestsSquared = sumOfRestsSquared &
                                + (velocityArray(:,i)-vel_avg) ** 2
        end do 

        sd = (sumOfRestsSquared / dfloat(numberOfWDs)) ** 0.5

    end subroutine int_calculateSigma


    subroutine vector_calculateSigma(sum, &
                                  numberOfWDs, &
                                  velocityArray, &
                                  vel_avg, &
                                  sd)
        real*8, dimension(3), intent(in) :: sum
        integer, dimension(3), intent(in) :: numberOfWDs
        real*8, dimension(:, :), intent(in) :: velocityArray
        real*8, dimension(3), intent(out) :: vel_avg
        real*8, dimension(3), intent(out) :: sd
        integer :: i, j
        real*8, dimension(3) :: sumOfRestsSquared &
                                = (/0.d0, 0.d0, 0.d0/)
        vel_avg = sum / dfloat(numberOfWDs)

        uvwLoop: do j = 1, 3
            do i = 1, numberOfWDs(j)
                sumOfRestsSquared(j) = sumOfRestsSquared(j) &
                                       + (velocityArray(j, i)-vel_avg(j)) &
                                       ** 2
            end do
        end do uvwLoop

        sd = (sumOfRestsSquared / dfloat(numberOfWDs)) ** 0.5

    end subroutine vector_calculateSigma

    
end module
