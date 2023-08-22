$PBExportHeader$w_acceso.srw
forward
global type w_acceso from w_acceso_usuario
end type
end forward

global type w_acceso from w_acceso_usuario
end type
global w_acceso w_acceso

event ue_datosempresa;call super::ue_datosempresa;String	empr_razsoc,empr_nombre,empr_rutemp,empr_direcc,empr_comuna,empr_ciudad,empr_giroem,empr_nrotel, &
			empr_nrofax,empr_repleg,empr_rutrle,empr_oficin,empr_disco
Long		ll_Tempor

SELECT   empr_razsoc,empr_nombre,empr_rutemp,empr_direcc,empr_comuna,empr_ciudad,empr_giroem,
		   	empr_nrotel,empr_nrofax,empr_repleg,empr_rutrle,empr_codexp,empr_codplt,empr_oficin,
		   	empr_disco,empr_especi,empr_varied,empr_embala,empr_passwd,empr_coacce,empr_varrot,empr_repale,
		   	empr_codgen,empr_codtra,empr_packing,empr_stusda,empr_fecsem,empr_recaja,empr_todclie,
			empr_calrot,empr_prorot,empr_pakrot,empr_pfxpal,empr_ctlven ,empr_clibas, empr_packing
	INTO  :empr_razsoc,:empr_nombre,:empr_rutemp,:empr_direcc,:empr_comuna,:empr_ciudad,&
			:empr_giroem,:empr_nrotel,:empr_nrofax,:empr_repleg,:empr_rutrle,:gi_codexport,&
			:gi_codplanta,:empr_oficin,:gs_disco,:gi_CodEspecie,:gi_CodVariedad,:gs_CodEmbalaje,:gs_Password,&
			:gi_controlacceso,:gi_vari_rotulada,:gi_Repale,&
			:gi_codgen,:gi_codtra,:gi_packing,:gi_stusda,:gd_fecultsemana,:gi_Rece_Caja,:gi_todclientes,
			:gi_cali_rotulado,:gi_prod_rotulado,:gi_pack_rotulado,:gs_pfijopallet,:gstr_parempresa.empr_ctlven,:gi_cliebase, :gi_Packing
	FROM dbo.parempresa;
	
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
gstr_apl.referencia		=	ProfileString(gstr_apl.ini, is_base, "Temporada", "")
gstr_apl.Oficina				=	empr_oficin

SELECT MAX(pate_tempor) INTO :ll_Tempor
FROM dbo.paramtemporada;

SELECT pate_inicio, pate_termin, pate_claliq
INTO :gd_TempoInicio,:gd_TempoTermin, :gs_clavecomext
FROM dbo.paramtemporada
WHERE pate_tempor = :ll_Tempor;

gs_Base					=	is_base

IF IsNull(gi_vari_rotulada) THEN gi_vari_rotulada = 0
IF IsNull(gi_repale) THEN gi_repale = 2

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
string picturename = "\Desarrollo 17\Imagenes\Sistemas\fruta_procesada.jpg"
end type

