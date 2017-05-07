module plot_uvw
    use, intrinsic :: iso_fortran_env, dp=>real64
    use derived_types, only: Star
    use files, only: getNewUnit

    implicit none

    private

    public :: plotUVWvsUVW

    interface plotUVWvsUVW
            module procedure threeSamples_plotUVWvsUVW
            module procedure oneSample_plotUVWvsUVW
    end interface plotUVWvsUVW
    
    character(len = *), parameter :: VW_OBS_PATH &
        = './outputs/observ/limoges/vw.dat'
    character(len = *), parameter :: UW_OBS_PATH &
        = './outputs/observ/limoges/uw.dat'
    character(len = *), parameter :: UV_OBS_PATH &
        = './outputs/observ/limoges/uv.dat'
    character(len = *), parameter :: VW_SYN_PATH &
        = './outputs/synth/limoges/vw.dat'
    character(len = *), parameter :: UW_SYN_PATH &
        = './outputs/synth/limoges/uw.dat'
    character(len = *), parameter :: UV_SYN_PATH &
        = './outputs/synth/limoges/uv.dat'
    character(len = *), parameter :: UVW_OBS_PATH &
        = './outputs/observ/no_crit/uvw.dat'
    character(len = *), parameter :: UVW_SYN_PATH &
        = './outputs/synth/no_crit/uvw.dat'
    character(len = *), parameter :: UV_FORMAT = '(2(f12.6,3x))'
    character(len = *), parameter :: UVW_FORMAT = '(3(f12.6,3x))'

contains


    subroutine oneSample_plotUVWvsUVW(sample, dataIsUsed)
        type (Star), dimension(:), intent(in) :: sample
        character(len = *), intent(in) :: dataIsUsed
        integer :: i
        integer :: unitUVW
        character(len = :), allocatable :: uvw_path

        select case (dataIsUsed)
            case("observational")
                uvw_path = UVW_OBS_PATH
            case("synthetic")
                uvw_path = UVW_SYN_PATH
            case default
                print *, "Error in oneSample_plotUVWvsUVW"
                stop
        end select

        open(getNewUnit(unitUVW), file = uvw_path, status='old')
        do i = 1, size(sample)
            write(unitUVW, UVW_FORMAT) sample(i)%vel(:)
        end do 
    end subroutine oneSample_plotUVWvsUVW


    subroutine threeSamples_plotUVWvsUVW(sampleUvsV, &
                                         sampleUvsW, &
                                         sampleVvsW, &
                                         dataIsUsed)
        type (Star), dimension(:), allocatable, intent(in) :: sampleUvsV
        type (Star), dimension(:), allocatable, intent(in) :: sampleUvsW
        type (Star), dimension(:), allocatable, intent(in) :: sampleVvsW
        character(len = *), intent(in) :: dataIsUsed
        integer :: i
        integer :: unitUV, &
                   unitUW, &
                   unitVW
        character(len = :), allocatable :: uv_path, &
                                           uw_path, &
                                           vw_path

        select case (dataIsUsed)
            case("observational")
                uv_path = UV_OBS_PATH
                uw_path = UW_OBS_PATH
                vw_path = VW_OBS_PATH
            case("synthetic")
                uv_path = UV_SYN_PATH
                uw_path = UW_SYN_PATH
                vw_path = VW_SYN_PATH
            case default
                print *, "Error in oneSample_plotUVWvsUVW"
                stop
        end select

        if (allocated(sampleUvsV) .and. allocated(sampleUvsW) &
                                  .and. allocated(sampleVvsW)) then

            open(getNewUnit(unitUV), file = uv_path, status='old')
            do i = 1, size(sampleUvsV)
                write(unitUV, UV_FORMAT) sampleUvsV(i)%vel(1), &
                                             sampleUvsV(i)%vel(2)
            end do 

            open(getNewUnit(unitUW), file = uw_path, status='old')
            do i = 1, size(sampleUvsW)
                write(unitUW, UV_FORMAT) sampleUvsW(i)%vel(1), &
                                             sampleUvsW(i)%vel(2)
            end do 

            open(getNewUnit(unitVW), file = vw_path, status='old')
            do i = 1, size(sampleVvsW)
                write(unitVW, UV_FORMAT) sampleVvsW(i)%vel(1), &
                                             sampleVvsW(i)%vel(2)
            end do
        end if 
    end subroutine threeSamples_plotUVWvsUVW
end module plot_uvw
