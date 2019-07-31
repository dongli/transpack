module transport_common_mod

  use mesh_mod

  implicit none

  type grided_tracer_type
    real(8), allocatable :: values(:)
  contains
    procedure :: init => grided_tracer_init
    procedure :: clear => grided_tracer_clear
    final :: grided_tracer_final
  end type grided_tracer_type

contains

  subroutine grided_tracer_init(this, mesh)

    class(grided_tracer_type), intent(inout) :: this
    type(mesh_type), intent(in) :: mesh

    call this%clear()

    allocate(this%values(mesh%num_cell))

  end subroutine grided_tracer_init

  subroutine grided_tracer_clear(this)

    class(grided_tracer_type), intent(inout) :: this

    if (allocated(this%values)) deallocate(this%values)

  end subroutine grided_tracer_clear

  subroutine grided_tracer_final(this)

    type(grided_tracer_type), intent(inout) :: this

    call this%clear()

  end subroutine grided_tracer_final

end module transport_common_mod
