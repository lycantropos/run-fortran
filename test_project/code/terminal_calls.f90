module terminal_calls

    implicit none

    public :: readArguments

contains

    subroutine readArguments(obsDataIsUsed, synthDataIsUsed, limogesCritIsUsed)

        implicit none
        integer :: num_args, i
        character(len = 12), dimension(:), allocatable :: args
        logical, intent(out) :: obsDataIsUsed
        logical, intent(out) :: synthDataIsUsed
        logical, intent(out) :: limogesCritIsUsed
    
        obsDataIsUsed = .false.
        synthDataIsUsed = .false.
        limogesCritIsUsed = .false.
    
        num_args = command_argument_count()
    
        if (num_args .eq. 0) then
            print*, "You should specify what data you want to process:"
            call printHelp
            stop
        else
            allocate(args(num_args))
            do i = 1, num_args
                call get_command_argument(i, args(i))
                select case(args(i))
                    case ("-o")
                        obsDataIsUsed = .true.
                    case ("-s")
                        synthDataIsUsed = .true.
                    case ("-l")
                        limogesCritIsUsed = .true.
                    case default
                        print*, "Wrong argument. Please, use following:"
                        call printHelp
                        stop
                end select
            end do
        end if
    
        if (obsDataIsUsed .eqv. synthDataIsUsed) then
            print*, "Wrong argument sequence. Please, use following:"
            call printHelp
            stop
        endif
    
    end subroutine readArguments


    subroutine printHelp()

        implicit none
        print*, "   -o - for observational data without Limoges criterion"
        print*, "   -o -l - for observational data with applied Limoges criterion"
        print*, "   -s - for data from population synthesis code without &
                        &Limoges criterion"
        print*, "   -s -l - for data from population synthesis code with &
                            &applied criterion of Limoges"

    end subroutine printHelp
    
end module terminal_calls