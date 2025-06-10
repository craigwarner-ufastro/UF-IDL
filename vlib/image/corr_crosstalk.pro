function corr_crosstalk, image, CFAC=cfac

  return, corr_eghosts_deconv( image, CFAC=cfac )
end
