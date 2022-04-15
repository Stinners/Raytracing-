module mod_hittable 
    use mod_ray, only: Ray
    implicit none 

    private 
    public hit_record_t
    public hittable

    type :: hit_record_t 
        real(8) :: point(3), normal(3), t
    end type hit_record_t

    type, abstract :: hittable
    contains 
        procedure (has_hit), deferred :: hit 
    end type hittable 

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

    
