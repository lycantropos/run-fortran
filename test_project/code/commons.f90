module commons


    implicit none

    public :: INPUT_PATH, &
              OUTPUT_FORMAT, &
              UVW_FORMAT
        
        ! TODO: Make a reference for the next path
        character(len = *), parameter :: INPUT_PATH = '/home/georgy&
        											  &/Documents/program&
        											  &/WD_population_40pc&
        											  &/output_data&
                                              		  &/boot_rowell_thin_1.out'
        character(len = *), parameter :: OUTPUT_FORMAT = '(2(f12.6,3x))'
        character(len = *), parameter :: UVW_FORMAT = '(3(f12.6,3x))'


contains


end module