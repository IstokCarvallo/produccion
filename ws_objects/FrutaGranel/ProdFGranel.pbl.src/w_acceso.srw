$PBExportHeader$w_acceso.srw
forward
global type w_acceso from w_acceso_usuario
end type
end forward

global type w_acceso from w_acceso_usuario
end type
global w_acceso w_acceso

type variables

end variables

event ue_datosempresa;call super::ue_datosempresa;String	empr_razsoc, empr_nombre, empr_rutemp, empr_direcc, empr_comuna, &
			empr_ciudad, empr_giroem, empr_nrotel, empr_nrofax, empr_repleg, &
			empr_rutrle, empr_oficin, empr_dirres
Integer  empr_admenv			

SELECT	empr_razsoc, empr_nombre, empr_rutemp, empr_direcc, empr_comuna,
			empr_ciudad, empr_giroem, empr_nrotel, empr_nrofax, empr_repleg,
			empr_rutrle, empr_oficin, empr_dirres, expo_codigo, empr_especi,
			empr_admenv, empr_passwd	
	INTO	:empr_razsoc, :empr_nombre, :empr_rutemp, :empr_direcc, :empr_comuna,
			:empr_ciudad, :empr_giroem, :empr_nrotel, :empr_nrofax, :empr_repleg,
			:empr_rutrle, :empr_oficin, :empr_dirres, :gi_codexport, :gi_CodEspecie,
			:empr_admenv, :gs_Password
	FROM	dbo.parempresa;
	
gstr_apl.razon_social		=	empr_razsoc
gstr_apl.nom_empresa	=	empr_nombre
gstr_apl.rut_empresa		=	empr_rutemp
gstr_apl.dir_empresa		=	empr_direcc
gstr_apl.com_empresa	=	empr_comuna
gstr_apl.ciu_empresa		=	empr_ciudad
gstr_apl.gir_empresa		=	empr_giroem
gstr_apl.tel_empresa		=	empr_nrotel
gstr_apl.fax_empresa		=	empr_nrofax
gstr_apl.rep_legal			=	empr_repleg
gstr_apl.rut_replegal		=	empr_rutrle
gstr_apl.Oficina				=	empr_oficin
gstr_apl.DirRespaldo		=	empr_dirres
gi_admenvase				=	empr_admenv

gs_Base					=	is_base


end event

on w_acceso.create
call super::create
end on

on w_acceso.destroy
call super::destroy
end on

type p_aceptar from w_acceso_usuario`p_aceptar within w_acceso
end type

type p_cerrar from w_acceso_usuario`p_cerrar within w_acceso
end type

type sle_nombre from w_acceso_usuario`sle_nombre within w_acceso
end type

type p_1 from w_acceso_usuario`p_1 within w_acceso
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type ddlb_bases from w_acceso_usuario`ddlb_bases within w_acceso
end type

type st_titulo from w_acceso_usuario`st_titulo within w_acceso
end type

type st_empresa from w_acceso_usuario`st_empresa within w_acceso
end type

type st_conect from w_acceso_usuario`st_conect within w_acceso
end type

type sle_clave from w_acceso_usuario`sle_clave within w_acceso
end type

type st_2 from w_acceso_usuario`st_2 within w_acceso
end type

type st_1 from w_acceso_usuario`st_1 within w_acceso
end type

type p_mono from w_acceso_usuario`p_mono within w_acceso
string picturename = "\Desarrollo 17\Imagenes\Sistemas\fruta_granel.jpg"
end type

