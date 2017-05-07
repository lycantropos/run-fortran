module terminal_calls
    implicit none
    public :: readArguments
contains


    subroutine readArguments(obsDataIsUsed, &
                             synthDataIsUsed, &
                             limogesCritIsUsed, &
                             splittingNonDAFromDA)
        integer :: num_args, i
        character(len = 12), dimension(:), allocatable :: args
        logical, intent(out) :: obsDataIsUsed
        logical, intent(out) :: synthDataIsUsed
        logical, intent(out) :: limogesCritIsUsed
        logical, intent(out) :: splittingNonDAFromDA
    
        obsDataIsUsed = .false.
        synthDataIsUsed = .false.
        limogesCritIsUsed = .false.
        splittingNonDAFromDA = .false.
    
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
                    case ("-d")
                        splittingNonDAFromDA = .true.
                    case default
                        print*, "Wrong argument. Please, use following:"
                        call printHelp
                        stop
                end select
            end do
        end if
    
        if ((obsDataIsUsed .eqv. synthDataIsUsed) &
            .and. (.not. splittingNonDAFromDA)) then
            print *, "Wrong argument sequence. Please, use following:"
            call printHelp
            stop
        endif

        if (splittingNonDAFromDA .eqv. (obsDataIsUsed .or. synthDataIsUsed &
                                        .or. limogesCritIsUsed)) then
            print *, "Calculating velocities separately for DA and nonDA WDs &
                      &now is only available for data from Limoges article &
                      &and no selection criteria cannot be applied for the &
                      &moment."
            call printHelp
            stop
        end if

    end subroutine readArguments


    subroutine printHelp()
        print *, "   -o - for observational data without Limoges criterion"
        print *, "   -o -l - for observational data with applied Limoges &
                             &criterion"
        print *, "   -s - for data from population synthesis code without &
                          &Limoges criterion"
        print *, "   -s -l - for data from population synthesis code with &
                             &applied criterion of Limoges"
        print *, "   -d - to get velocities for DA and nonDA WDs separately &
                          &from Limoges article"                            
    end subroutine printHelp
end module terminal_calls