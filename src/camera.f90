module mod_camera 
    use mod_ray, only: Ray
    implicit none 

    type :: Camera_t
        real(8) :: aspect_ratio, viewport_height, viewport_width, focal_length
        real(8) :: origin(3), horizontal(3), vertical(3), lower_left_corner(3)
    contains 
        procedure :: get_ray
    end type Camera_t

contains 

    pure function init_camera() result(new)
        type(Camera_t) :: new
        
        new % aspect_ratio = 16.0 / 9.0
        new % viewport_height = 2.0
        new % viewport_width = new % aspect_ratio * new % viewport_height
        new % focal_length = 1.0

        new % origin  = [0.0, 0.0, 0.0]
        new % horizontal = [new % viewport_width, 0.0_8, 0.0_8]
        new % vertical = [0.0_8, new % viewport_height, 0.0_8]
        new % lower_left_corner = new%origin - (new%horizontal)/2 - (new%vertical)/2 - [0.0_8, 0.0_8, new%focal_length]
    end function init_camera

    pure function get_ray(self, u, v) result(new_ray)
        class(Camera_t), intent(in) :: self
        real(8), intent(in) :: u, v
        type(Ray) :: new_ray 

        new_ray % origin = self % origin 
        new_ray % direction = self%lower_left_corner + u*self%horizontal + v*self%vertical - self%origin
    end function get_ray

end module mod_camera 
