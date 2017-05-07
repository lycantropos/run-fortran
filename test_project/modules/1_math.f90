module math

    use, intrinsic :: iso_fortran_env, dp=>real64

    implicit none

    private

    public :: PI, &
              multiplyMatrixByVector, &
              getSD

    real(dp), parameter :: PI = 4.0_dp * datan(1.0_dp)
    ! TODO: add test as for different PCs NaN code can be different
    real(dp), parameter :: NAN_64 = transfer(-2251799813685248_int64, 1._dp)

contains


    function multiplyMatrixByVector(A,x) result(y)

        real(dp),dimension(3,3),intent(in) :: A
        real(dp),dimension(3),intent(in) :: x
        real(dp),dimension(3) :: y
        ! TODO: rewrite it with elemental operations
        y(1) = A(1, 1)*x(1) + A(1, 2)*x(2) + A(1, 3)*x(3)
        y(2) = A(2, 1)*x(1) + A(2, 2)*x(2) + A(2, 3)*x(3)
        y(3) = A(3, 1)*x(1) + A(3, 2)*x(2) + A(3, 3)*x(3)

    end function

     
    function getSD(array) result(sD)
        real(dp), dimension(:), intent(in) :: array
        real(dp) :: sD
        real(dp) :: avg
        real(dp) :: sumOfRestsSquared = 0.0_dp

        avg = sum(array) / size(array)
        sumOfRestsSquared = sum((array(:) - avg) ** 2)
        sD = sqrt(sumOfRestsSquared / size(array))
        ! NOTE: this shouldn't be here. What shoud we do when we have 
        ! only 1 element?
        if (size(array) == 1) then
            sD = 100.0_dp
        end if
    end function getSD
end module
