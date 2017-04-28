module criterion

    use, intrinsic :: iso_fortran_env

    implicit none

    public :: applyLimogesCriterion

contains

    subroutine applyLimogesCriterion(coordOfWD, &
                                     vel_hel, &
                                     counterOfWD, &
                                     vel_sum, &
                                     velocityArray, &
                                     highestCoord)
        implicit none
        real(real64), dimension(3), intent(in) :: coordOfWD
        real(real64), dimension(3), intent(in) :: vel_hel
        real(real64), dimension(3), intent(inout) :: vel_sum
        real(real64), dimension(:,:), intent(inout) :: velocityArray
        integer, dimension(3), intent(inout) :: counterOfWD
        character(len=1), intent(out) :: highestCoord
        real(real64) :: highestCoordValue

        highestCoordValue = maxval(abs(coordOfWD))

        ! Highest is X
        if (abs(highestCoordValue - abs(coordOfWD(1))) .lt. 1.0d-5) then
            counterOfWD(2) = counterOfWD(2) + 1  ! V
            counterOfWD(3) = counterOfWD(3) + 1  ! W

            vel_sum(2) = vel_sum(2) + vel_hel(2)
            vel_sum(3) = vel_sum(3) + vel_hel(3)  

            velocityArray(2, counterOfWD(2)) = vel_hel(2)
            velocityArray(3, counterOfWD(3)) = vel_hel(3)

            highestCoord = "x"
        ! Highest is Y
        else if (abs( highestCoordValue - abs(coordOfWD(2) )) .lt. 1.0d-5) then
            counterOfWD(1) = counterOfWD(1) + 1  ! U
            counterOfWD(3) = counterOfWD(3) + 1  ! W

            vel_sum(1) = vel_sum(1) + vel_hel(1)
            vel_sum(3) = vel_sum(3) + vel_hel(3)

            velocityArray(1, counterOfWD(1)) = vel_hel(1)
            velocityArray(3, counterOfWD(3)) = vel_hel(3)

            highestCoord = "y"
        ! Highest is Z
        else if (abs( highestCoordValue - abs(coordOfWD(3) )) .lt. 1.0d-5) then
            counterOfWD(1) = counterOfWD(1) + 1  ! U
            counterOfWD(2) = counterOfWD(2) + 1  ! V

            vel_sum(1) = vel_sum(1) + vel_hel(1)
            vel_sum(2) = vel_sum(2) + vel_hel(2)

            velocityArray(1, counterOfWD(1)) = vel_hel(1)
            velocityArray(2, counterOfWD(2)) = vel_hel(2)

            highestCoord = "z"
        else
            print*, "Error: couldn't determine highest coordinate."
        end if

    end subroutine applyLimogesCriterion
    
end module criterion