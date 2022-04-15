module mod_hittable 
    use mod_ray, only: Ray
    implicit none 

    private 
    public hit_record_t
    public hittable
    public material_t

    type, abstract :: material_t
    contains 
        procedure (abstract_scatter) , deferred :: scatter 
    end type material_t

    type :: hit_record_t 
        real(8) :: point(3), normal(3), t
        class(material_t), pointer :: mat_ptr
    end type hit_record_t

    type, abstract :: hittable
    contains 
        procedure (has_hit), deferred :: hit 
    end type hittable 

    interface 
        logical function abstract_scatter(self, r, record, attenuation, scattered)
            import :: material_t, Ray, hit_record_t
            class(material_t), intent(in) :: self
            class(Ray), intent(in) :: r
            class(hit_record_t), intent(in) :: record
            real(8), intent(out) :: attenuation(3)
            type(Ray), intent(out) :: scattered
        end function abstract_scatter
    end interface 

    interface 
        logical function has_hit(self, r, t_min, t_max, hit_record)
            import :: hit_record_t, hittable, Ray
            class(hittable), intent(in) :: self
            type(Ray), intent(in) :: r 
            real(8), intent(in) :: t_min, t_max
            class(hit_record_t), intent(out) :: hit_record
        end function has_hit
    end interface 

end module mod_hittable

    
