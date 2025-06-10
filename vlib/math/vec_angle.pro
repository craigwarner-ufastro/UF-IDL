function Vec_Angle ,vf1,vf2

return,!radeg * acos( Vec_Dot_Prod( vf1, vf2 ) / (Vec_Norm(vf1)*Vec_Norm(vf2)) )
end
