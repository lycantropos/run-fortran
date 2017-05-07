! This module treats data from population synthesis code and applies
! criterion from Limoges et al. 2015
module synthetic
    use, intrinsic :: iso_fortran_env, dp=>real64
    use files, only: getNumberOfLines, &
                     getNewUnit
    use derived_types, only: Star
    use criterion, only: splitDataForUVWvsUVW, &
                         splitDataForUVWvsMbol
    use plot_uvw, only: plotUVWvsUVW
    use plot_mbol, only: plotUVWvsMbol
    use math, only: getSD
    implicit none

    private

    public :: treatSynthData

        character(len = *), parameter :: INPUT_PATH &
            = '/home/georgy/Documents/program/WD_population_40pc/output_data&
              &/boot_rowell_thin_1.out'
contains

    subroutine treatSynthData(limogesCriterionIsUsed)

        logical, intent(in) :: limogesCriterionIsUsed
        integer :: numberOfWDs, &
                   unitInput, &
                   i
        type (Star), dimension(:), allocatable :: whiteDwarfs
        real*8, dimension(22) :: inputData
        type (Star), dimension(:), allocatable :: sampleUvsV, &
                                                  sampleUvsW, &
                                                  sampleVvsW, &
                                                  sampleUvsMbol, &
                                                  sampleVvsMbol, &
                                                  sampleWvsMbol

        numberOfWDs = getNumberOfLines(INPUT_PATH)

        allocate(whiteDwarfs(numberOfWDs))

        print *, "Data from population synthesis code"
        print *, "Number of White Dwarfs:", numberOfWDs

        open(getNewUnit(unitInput), file = INPUT_PATH, status='old')

        do i = 1, numberOfWDs
            read(unitInput, *) inputData
            whiteDwarfs(i)%magnitude = inputData(4)
            whiteDwarfs(i)%rightAscension = inputData(10)
            whiteDwarfs(i)%declination = inputData(11)
            whiteDwarfs(i)%distance = inputData(12) * 1.d3  ! kpc to pc
            whiteDwarfs(i)%vel = inputData(20:22)
        end do

        call whiteDwarfs(:)%equatToGalact
        call whiteDwarfs(:)%galactToXYZ

        if (limogesCriterionIsUsed) then
            print *, "Limoges criterion is used"
            call splitDataForUVWvsUVW(whiteDwarfs, &
                                      sampleUvsV, &
                                      sampleUvsW, &
                                      sampleVvsW)
            call splitDataForUVWvsMbol(whiteDwarfs, &
                                       sampleUvsMbol, &
                                       sampleVvsMbol, &
                                       sampleWvsMbol)
            call plotUVWvsUVW(sampleUvsV, &
                              sampleUvsW, &
                              sampleVvsW, &
                              "synthetic")
            call plotUVWvsMbol(sampleUvsMbol, &
                               sampleVvsMbol, &
                               sampleWvsMbol, &
                               "synthetic")
            print *, "Average velocity components:", &
                sum(sampleUvsMbol(:)%vel(1)) / size(sampleUvsMbol), &
                sum(sampleVvsMbol(:)%vel(2)) / size(sampleVvsMbol), &
                sum(sampleWvsMbol(:)%vel(3)) / size(sampleWvsMbol)
            print *, "Standart deviations:        ", &
                getSD(sampleUvsMbol(:)%vel(1)), &
                getSD(sampleVvsMbol(:)%vel(2)), &
                getSD(sampleWvsMbol(:)%vel(3))
        else 
            call plotUVWvsUVW(whiteDwarfs, "synthetic")
            call plotUVWvsMbol(whiteDwarfs, "synthetic")
            print *, "Average velocity components:", &
                sum(whiteDwarfs(:)%vel(1)) / size(whiteDwarfs), &
                sum(whiteDwarfs(:)%vel(2)) / size(whiteDwarfs), &
                sum(whiteDwarfs(:)%vel(3)) / size(whiteDwarfs)
            print *, "Standart deviations:        ", &
                getSD(whiteDwarfs(:)%vel(1)), &
                getSD(whiteDwarfs(:)%vel(2)), &
                getSD(whiteDwarfs(:)%vel(3))
        end if
    end subroutine
end module
