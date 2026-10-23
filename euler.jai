EulerAngles :: struct {
    pitch, yaw, roll: float;

    #overlay (pitch) using xyz: Vec3;
}