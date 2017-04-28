! This program reads observational data from Limoges et al. 2015 or 
! data from population synthesis code and calculates average 
! velocities, standart deviation and for observational data - 
! characteristics of bolometric magnitudes bins 
program velocities

    use observational, only: treatObservData
    use synthetic, only: treatSynthData 
    use terminal_calls, only: readArguments
  
    implicit none
    logical :: obsDataIsUsed, &
               syntDataIsUsed, &
               limogesCriterionFlag

    call readArguments(obsDataIsUsed, syntDataIsUsed, limogesCriterionFlag)

    if (obsDataIsUsed) then
        call treatObservData(limogesCriterionFlag)
    else if (syntDataIsUsed) then
        call treatSynthData(limogesCriterionFlag)
    else
        print*, "Critical error: check 'subroutine readArguments'"
    end if

stop
end program
