
#scope_export
Mesh2D :: MeshInternalType(Vec2);
Mesh3D :: MeshInternalType(Vec3);

get_mesh_triangle_count :: inline (mesh: $T/MeshTypes) -> s64 {
    num_triangle_points := ifx mesh.indices.count > 0 then mesh.indices.count else mesh.vertices.count;
    return num_triangle_points / 3;
}

get_mesh_triangle_point_indices :: inline (mesh: MeshInternalType($PointType), triangle_index: int) -> [3] int {
    result: [3] int;

    for i: 0..2 {
        vertex := triangle_index * 3 + i;
        if mesh.indices.count > 0 {
            vertex = mesh.indices[vertex];
        }

        result[i] = vertex;
    }

    return result;
}

get_mesh_triangle_points :: inline (mesh: MeshInternalType($PointType), index: int) -> [3] PointType {
    result: [3] PointType;

    for i: 0..2 {
        vertex := index * 3 + i;
        if mesh.indices.count > 0 {
            vertex = mesh.indices[vertex];
        }

        result[i] = mesh.vertices[vertex];
    }

    return result;
}

#scope_module
MeshTypes :: Type.[Mesh2D, Mesh3D];

MeshInternalType :: struct ($T: Type) {
    vertices: [] T;
    indices: [] u32;
}
