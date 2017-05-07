module derived_types
    use, intrinsic :: iso_fortran_env, dp => real64
    use math, only: PI
    use astronomy, only: convertEquatorToGalact, &
                         convertGalacticToXYZ, &
                         convertEquatorMotionToUVW
    implicit none

    private

    public :: Star, &
              JaggedArray

    type Star
        real(dp) :: distance, &
                    rightAscension, &
                    declination, &
                    motionInRA, &
                    motionInDEC, &
                    magnitude, &
                    lattitude, &
                    longitude
        real(dp), dimension(3) :: coords, &
                                  vel
        character(len = 5) :: spectralType
    contains
        procedure :: equatToGalact => star_equatToGalact
        procedure :: galactToXYZ => star_galactToXYZ
        procedure :: equatToUVW => star_equatToUVW
    end type

    type JaggedArray
        type(Star), dimension(:), allocatable :: row
    end type JaggedArray

contains


    impure elemental subroutine star_equatToGalact(this)
        class (Star), intent(inout) :: this

        if ((abs(this%rightAscension) > 2.0_dp*PI) .or. &
            (abs(this%declination) > 2.0_dp*PI)) stop "Error: wrong angle &
            &input in star_equatToGalact"

        call convertEquatorToGalact(this%rightAscension, &
                                    this%declination, &
                                    this%longitude, &
                                    this%lattitude)
    end subroutine star_equatToGalact
    

    impure elemental subroutine star_galactToXYZ(this)
        class(Star), intent(inout) :: this

        ! TODO: check why I have an angle more than 2pi
        ! print *, i, this%longitude, this%lattitude
        ! i = i + 1
        ! if ((abs(this%longitude) > 2.0_dp*PI) .or. &
        !     (abs(this%lattitude) > 2.0_dp*PI)) stop "Error: wrong angle &
        !     &input in star_galactToXYZ"

        this%coords = convertGalacticToXYZ(this%distance, &
                                           this%longitude, &
                                           this%lattitude) 
    end subroutine star_galactToXYZ


    impure elemental subroutine star_equatToUVW(this)
        class(Star), intent(inout) :: this

        call convertEquatorMotionToUVW(this%rightAscension, &
                                       this%declination, &
                                       this%distance, &
                                       this%motionInDEC, &
                                       this%motionInRA, &
                                       this%vel)
    end subroutine star_equatToUVW

end module derived_types