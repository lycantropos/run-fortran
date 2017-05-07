! This program reads observational data from Limoges et al. 2015 or 
! data from population synthesis code and calculates average 
! velocities, standart deviation and for observational data - 
! characteristics of bolometric magnitudes bins 
program main

    use observational, only: treatObservData
    use synthetic, only: treatSynthData 
    use terminal_calls, only: readArguments
  
    implicit none
    logical :: obsDataIsUsed, &
               syntDataIsUsed, &
               limogesCriterionFlag, &
               splittingNonDAFromDAFlag

    call readArguments(obsDataIsUsed, &
                       syntDataIsUsed, &
                       limogesCriterionFlag, &
                       splittingNonDAFromDAFlag)

    if (obsDataIsUsed) then
        call treatObservData(limogesCriterionFlag, &
                             splittingNonDAFromDAFlag)
    else if (syntDataIsUsed) then
        call treatSynthData(limogesCriterionFlag)
    else if (splittingNonDAFromDAFlag) then
        call treatObservData(limogesCriterionFlag, &
                             splittingNonDAFromDAFlag)
    else
        print*, "Critical error: check 'subroutine readArguments'"
    end if

stop
end program main
