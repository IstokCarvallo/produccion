$PBExportHeader$w_info_opcion_ctrol_cali_recepcion.srw
$PBExportComments$opciones para desplagar el informe de análisis de control de calidad en recepción
forward
global type w_info_opcion_ctrol_cali_recepcion from w_para_informes
end type
type st_1 from statictext within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_transporte from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_calibre from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
type st_2 from statictext within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_categoria from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_color from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_madurez from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_cosecha from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
type cbx_control from checkbox within w_info_opcion_ctrol_cali_recepcion
end type
end forward

global type w_info_opcion_ctrol_cali_recepcion from w_para_informes
integer width = 2761
integer height = 1080
string title = "Opciónes para Informe Control de Calidad en Recepción"
st_1 st_1
cbx_transporte cbx_transporte
cbx_calibre cbx_calibre
st_2 st_2
cbx_categoria cbx_categoria
cbx_color cbx_color
cbx_madurez cbx_madurez
cbx_cosecha cbx_cosecha
cbx_control cbx_control
end type
global w_info_opcion_ctrol_cali_recepcion w_info_opcion_ctrol_cali_recepcion

type variables
str_busqueda	istr_busq

uo_variedades iuo_variedad
uo_parammadurez iuo_madurez
uo_colordefondo   iuo_colorfondo
end variables

forward prototypes
public function boolean ExisteColorfondo (integer ai_especie, integer ai_grupo, integer ai_subgrupo, integer ai_variedad)
end prototypes

public function boolean ExisteColorfondo (integer ai_especie, integer ai_grupo, integer ai_subgrupo, integer ai_variedad);Integer li_Cantidad

SELECT	COUNT(*)
	INTO	:li_Cantidad
	FROM	dba.spro_colordefondo
	WHERE	espe_codigo	=	:ai_especie
	AND   isnull(grva_codigo,-1) =  :ai_grupo
	AND   isnull(grva_codsub,-1) =  :ai_subgrupo
	AND	isnull(vari_codigo,-1) =  :ai_Variedad;
	
	
IF isnull(li_Cantidad) OR li_cantidad=0 THEN
	RETURN FALSE
END IF

RETURN TRUE


end function

on w_info_opcion_ctrol_cali_recepcion.create
int iCurrent
call super::create
this.st_1=create st_1
this.cbx_transporte=create cbx_transporte
this.cbx_calibre=create cbx_calibre
this.st_2=create st_2
this.cbx_categoria=create cbx_categoria
this.cbx_color=create cbx_color
this.cbx_madurez=create cbx_madurez
this.cbx_cosecha=create cbx_cosecha
this.cbx_control=create cbx_control
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.cbx_transporte
this.Control[iCurrent+3]=this.cbx_calibre
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.cbx_categoria
this.Control[iCurrent+6]=this.cbx_color
this.Control[iCurrent+7]=this.cbx_madurez
this.Control[iCurrent+8]=this.cbx_cosecha
this.Control[iCurrent+9]=this.cbx_control
end on

on w_info_opcion_ctrol_cali_recepcion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.cbx_transporte)
destroy(this.cbx_calibre)
destroy(this.st_2)
destroy(this.cbx_categoria)
destroy(this.cbx_color)
destroy(this.cbx_madurez)
destroy(this.cbx_cosecha)
destroy(this.cbx_control)
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm

iuo_variedad 	 = CREATE uo_variedades

iuo_madurez 	 = CREATE uo_parammadurez

iuo_colorfondo  = CREATE uo_colordefondo


end event

type st_titulo from w_para_informes`st_titulo within w_info_opcion_ctrol_cali_recepcion
integer y = 136
integer width = 1993
string text = "Control de Calidad en Recepción "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_opcion_ctrol_cali_recepcion
integer x = 2414
integer y = 268
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila, ll_filas
Integer	li_Control, li_grupo, li_subgrupo, li_varieco, li_grupo1, li_subgrupo1, li_varieco1
DataStore	lds_Base

li_varieco  =  Integer(istr_busq.Argum[4])							  
li_varieco1 =  Integer(istr_busq.Argum[4])
IF iuo_variedad.existe(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[4]),&
                       TRUE,SQLCA) THEN
	li_grupo 	= 	iuo_variedad.grupo
	li_grupo1   =  iuo_variedad.grupo
	li_subgrupo	=	iuo_variedad.subgrupo
	li_subgrupo1=	iuo_variedad.subgrupo
ELSE
	SetNull(li_grupo)
	SetNull(li_subgrupo)
	SetNull(li_grupo1)
	SetNull(li_subgrupo1)
END IF

IF NOT existecolorfondo(Integer(istr_busq.Argum[2]),li_grupo,li_subgrupo,&
											li_varieco) THEN
	li_varieco = -1										
	IF NOT existecolorfondo(Integer(istr_busq.Argum[2]),li_grupo,li_subgrupo,-1) THEN
		li_subgrupo = -1								
		IF NOT existecolorfondo(Integer(istr_busq.Argum[2]),li_grupo,-1,-1) THEN
			li_grupo = -1								
			IF NOT existecolorfondo(Integer(istr_busq.Argum[2]),-1,-1,-1) THEN
			END IF
		END IF
	END IF
END IF	
	
IF NOT iuo_Madurez.Existe(Integer(istr_busq.Argum[2]),li_grupo1,li_subgrupo1,&
											li_varieco1, &
											FALSE, Sqlca) THEN
	li_varieco1 = -1										
	IF NOT iuo_Madurez.Existe(Integer(istr_busq.Argum[2]),li_grupo1,li_subgrupo1,&
											-1, &
											FALSE, Sqlca) THEN
		li_subgrupo1 = -1								
		IF NOT iuo_Madurez.Existe(Integer(istr_busq.Argum[2]),li_grupo1,-1,&
											-1, &
											FALSE, Sqlca) THEN
			li_grupo1 = -1								
			IF NOT iuo_Madurez.Existe(Integer(istr_busq.Argum[2]),-1,-1,&
											-1, &
											FALSE, Sqlca) THEN
			END IF
		END IF
	END IF
END IF	
	
	
	
istr_info.titulo	= "ANALISIS CONTROL DE CALIDAD EN RECEPCION"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)


vinf.dw_1.DataObject = "dw_info_anctrcal_recepcion"

IF cbx_control.Checked THEN li_Control = 1

IF NOT cbx_transporte.Checked THEN
	vinf.dw_1.object.dw_recep_transporte.visible = 0
END IF 

IF NOT cbx_calibre.checked THEN
	vinf.dw_1.object.dw_recep_calibres.visible = 0
END IF 
	
IF NOT cbx_categoria.checked THEN
	vinf.dw_1.object.dw_recep_categorias.visible = 0
END IF 
	
IF NOT cbx_color.checked THEN
	vinf.dw_1.object.dw_recep_color.visible = 0
END IF 

IF NOT cbx_madurez.checked THEN
	vinf.dw_1.object.dw_recep_madurez.visible = 0
END IF 

IF NOT cbx_cosecha.checked THEN
	vinf.dw_1.object.dw_recep_calidad.visible = 0
	vinf.dw_1.object.dw_recep_condicion.visible = 0
	vinf.dw_1.object.dw_suma_dano.visible = 0
	vinf.dw_1.object.dw_danos_otrascat.visible = 0
END IF

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_busq.Argum[1]), &
								  Integer(istr_busq.Argum[2]), &
								  Integer(istr_busq.Argum[3]), &
								  Integer(istr_busq.Argum[4]), &
								  li_grupo, li_subgrupo,li_varieco,&
								  li_grupo1, li_subgrupo1,li_varieco1,&
								  li_control)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_opcion_ctrol_cali_recepcion
integer x = 2414
integer y = 632
end type

type st_1 from statictext within w_info_opcion_ctrol_cali_recepcion
integer x = 151
integer y = 368
integer width = 2094
integer height = 540
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_transporte from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 229
integer y = 432
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Condición de Transporte"
boolean checked = true
end type

type cbx_calibre from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 1248
integer y = 432
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Distribución de Calibre"
boolean checked = true
end type

type st_2 from statictext within w_info_opcion_ctrol_cali_recepcion
integer x = 151
integer y = 264
integer width = 2094
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Opciones de Impresión"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_categoria from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 229
integer y = 548
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Distribución de Categorías "
boolean checked = true
end type

type cbx_color from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 1248
integer y = 548
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Distribución del Color"
boolean checked = true
end type

type cbx_madurez from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 229
integer y = 664
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Madurez de Cosecha"
boolean checked = true
end type

type cbx_cosecha from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 1248
integer y = 664
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Daños y Defectos"
boolean checked = true
end type

type cbx_control from checkbox within w_info_opcion_ctrol_cali_recepcion
integer x = 475
integer y = 780
integer width = 1449
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Calificación de Mercado y Fecha de Recepción"
boolean checked = true
end type

