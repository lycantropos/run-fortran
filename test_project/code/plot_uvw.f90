module plot_uvw


    use commons, only: OUTPUT_FORMAT, &
                       UVW_FORMAT

    implicit none

    public :: fillSelectedDataForUVWPlot, &
              fillFullDataForUVWPlot


contains


    subroutine fillSelectedDataForUVWPlot(highest, &
                                          vel_hel, &
                                          counter, &
                                          vel_sum, &
                                          vel_array, &
                                          unitVW, &
                                          unitUW, &
                                          unitUV)
        
        character(len = 1), intent(in) :: highest
        real*8, dimension(3), intent(in) :: vel_hel
        integer, dimension(3), intent(inout) :: counter
        real*8, dimension(3), intent(inout) :: vel_sum
        real*8, dimension(:, :), intent(inout) :: vel_array
        ! TODO: hide this somewhere
        integer, intent(in) :: unitVW, &
                               unitUW, &
                               unitUV
        
        plot_UV_or_UW: if (highest .ne. "x") then
            counter(1) = counter(1) + 1  ! U
            vel_sum(1) = vel_sum(1) + vel_hel(1)
            vel_array(1, counter(1)) = vel_hel(1)
        end if plot_UV_or_UW
        plot_UV_or_VW: if (highest .ne. "y") then
            counter(2) = counter(2) + 1  ! V
            vel_sum(2) = vel_sum(2) + vel_hel(2)
            vel_array(2, counter(2)) = vel_hel(2)
        end if plot_UV_or_VW
        plot_UW_or_VW: if (highest .ne. "z") then
            counter(3) = counter(3) + 1  ! W
            vel_sum(3) = vel_sum(3) + vel_hel(3)
            vel_array(3, counter(3)) = vel_hel(3)
        end if plot_UW_or_VW

        select case (highest)
            case("x")
                write(unitVW, OUTPUT_FORMAT) vel_hel(2), &
                                             vel_hel(3)
            case("y")
                write(unitUW, OUTPUT_FORMAT) vel_hel(1), &
                                             vel_hel(3)
            case("z")
                write(unitUV, OUTPUT_FORMAT) vel_hel(1), &
                                             vel_hel(2)
            case default
                print*, "Error: couldn't determine highest coordinate"
        end select
        
    end subroutine fillSelectedDataForUVWPlot


    subroutine fillFullDataForUVWPlot(vel_hel, &
                                      counter, &
                                      vel_sum, &
                                      vel_array, &
                                      unitUVW)
        
        real*8, dimension(3), intent(in) :: vel_hel
        integer, intent(in) :: counter
        real*8, dimension(3), intent(inout) :: vel_sum
        real*8, dimension(:, :), intent(inout) :: vel_array
        ! TODO: hide this somewhere
        integer, intent(in) :: unitUVW

        vel_sum = vel_sum + vel_hel
        vel_array(:, counter) = vel_hel

        write(unitUVW, UVW_FORMAT) vel_hel
        
    end subroutine fillFullDataForUVWPlot


end module plot_uvw
