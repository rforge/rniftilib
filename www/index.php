
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="Rniftilib_logo.png" border="0" alt="Rniftilib Logo" /> </a> </td> </tr>
</table>

<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<h1>Welcome to the Rniftilib project!</h1>
<p>
<a href="http://r-forge.r-project.org/projects/rniftilib/">Rniftilib</a> provides an R-interface to the NIfTI reference implementation the <a href="http://niftilib.sourceforge.net/">niftilib C-library</a>.
</p>

<p>
The NIfTI reference implementation is a library for reading and writing files in the nifti-1 data format. 
The nifti-1 file format is a binary file format for storing medical image data and corresponding meta information,
e.g. transformation matrices, data type, image modality.
The image format is often used for analyzing magnetic resonance image (MRI) and functional MRI (fMRI) brain images.
</p>
<p>
Additional information about the reference implementation  and the <a href="http://nifti.nimh.nih.gov/nifti-1">NIfTI data format</a> can be found on the 
<a href="http://nifti.nimh.nih.gov">Neuroimaging Informatics Technology Initiative</a> homepage.
</p>

<!-- end of project description -->

<p>Further information as well as source and binary packages can be found on the <a href="http://r-forge.r-project.org/projects/rniftilib/">project summary page</a>.</p>
<img src="nifti_iconBlue_small.png" border="0" alt="* " />
</body>
</html>
