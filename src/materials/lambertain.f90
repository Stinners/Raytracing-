module mod_lambertian
    use mod_hittable, only: material_t, hit_record_t
    use mod_ray, only: Ray
    use mod_vec3, only: random_unit_vector, near_zero
    implicit none 

    type, extends(material_t) :: lambertian_t
        real(8) :: albedo(3)
    contains 
        procedure :: scatter
    end type lambertian_t

contains 

    logical function scatter(self, r, record, attenuation, scattered)
        class(lambertian_t), intent(in) :: self
        class(Ray), intent(in) :: r
        class(hit_record_t), intent(in) :: record
        real(8), intent(out) :: attenuation(3)
        type(Ray), intent(out) :: scattered

        real(8) :: scatter_direction(3)

        scatter_direction = record % normal + random_unit_vector()

        if (near_zero(scatter_direction)) then 
            scatter_direction = record % normal 
        end if 

        scattered = Ray(record % point, scatter_direction)
        attenuation = self % albedo
        scatter = .true.
    end function scatter

end module mod_lambertian
