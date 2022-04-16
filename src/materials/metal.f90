module mod_metal
    use mod_hittable, only: material_t, hit_record_t
    use mod_ray, only: Ray
    use mod_vec3, only: random_unit_vector, reflect, unit_vec, random_in_unit_sphere
    implicit none 

    type, extends(material_t) :: metal_t
        real(8) :: albedo(3), fuzz
    contains 
        procedure :: scatter
    end type metal_t

    ! TODO make a custom constructor for metal that contains the value of fuzz

contains 

    logical function scatter(self, r, record, attenuation, scattered)
        class(metal_t), intent(in) :: self
        class(Ray), intent(in) :: r
        class(hit_record_t), intent(in) :: record
        real(8), intent(out) :: attenuation(3)
        type(Ray), intent(out) :: scattered

        real(8) :: reflected(3)

        reflected = reflect(unit_vec(r % direction), record % normal)
        scattered = Ray(record%point, reflected + self%fuzz * random_in_unit_sphere())
        attenuation = self % albedo

        scatter = dot_product(scattered % direction, record % normal) > 0
    end function scatter

end module mod_metal
