$PBExportHeader$w_info_resumen_gral_insp_diaria.srw
$PBExportComments$Ventana Informe de Resumen General de Inspecciones Diarias.
forward
global type w_info_resumen_gral_insp_diaria from w_para_informes
end type
type st_4 from statictext within w_info_resumen_gral_insp_diaria
end type
type st_1 from statictext within w_info_resumen_gral_insp_diaria
end type
type dw_1 from datawindow within w_info_resumen_gral_insp_diaria
end type
type em_fechadesde from editmask within w_info_resumen_gral_insp_diaria
end type
type st_44 from statictext within w_info_resumen_gral_insp_diaria
end type
type st_12 from statictext within w_info_resumen_gral_insp_diaria
end type
type st_13 from statictext within w_info_resumen_gral_insp_diaria
end type
type em_fechahasta from editmask within w_info_resumen_gral_insp_diaria
end type
end forward

global type w_info_resumen_gral_insp_diaria from w_para_informes
integer x = 14
integer y = 32
integer width = 2606
integer height = 1244
string title = "Resumen General de Inspecciones diarias"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_1 dw_1
em_fechadesde em_fechadesde
st_44 st_44
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
end type
global w_info_resumen_gral_insp_diaria w_info_resumen_gral_insp_diaria

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zona

uo_plantadesp			iuo_plantadesp
end variables

on w_info_resumen_gral_insp_diaria.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_1=create dw_1
this.em_fechadesde=create em_fechadesde
this.st_44=create st_44
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_1
this.Control[iCurrent+4]=this.em_fechadesde
this.Control[iCurrent+5]=this.st_44
this.Control[iCurrent+6]=this.st_12
this.Control[iCurrent+7]=this.st_13
this.Control[iCurrent+8]=this.em_fechahasta
end on

on w_info_resumen_gral_insp_diaria.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_1)
destroy(this.em_fechadesde)
destroy(this.st_44)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
end on

event open;/***********************************

Argumento[1] = Código Cliente
Argumento[2] = Código Planta
Argumento[3] = Packing
Argumento[5] = Fecha Desde
Argumento[6] = Fecha Hasta

***********************************/

x=0
y=0

This.Icon	=	Gstr_apl.Icono


dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_1.InsertRow(0)
dw_1.SetItem(1,"plde_codigo", gi_codplanta)

istr_mant.argumento[1] 	=	String(gi_codexport)
istr_mant.argumento[2] 	=	String(gi_codplanta)
istr_mant.argumento[3] 	=	"2"
istr_mant.argumento[4] 	=	String(gi_codzona)
istr_mant.argumento[5] 	=	String(Today())
istr_mant.argumento[6] 	=	String(Today())
em_fechadesde.text		=	String(Today())
em_fechahasta.text		=	String(Today())


end event

type st_computador from w_para_informes`st_computador within w_info_resumen_gral_insp_diaria
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_gral_insp_diaria
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_gral_insp_diaria
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_gral_insp_diaria
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_gral_insp_diaria
integer x = 297
integer y = 252
integer width = 1934
string text = "Resumen General de Inspecciones Diarias"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_gral_insp_diaria
string tag = "Imprimir Reporte"
integer x = 2363
integer y = 476
integer taborder = 40
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Integer	li_fila, li_planta, li_cliente
Long		ll_planilla_sag

istr_info.titulo	= 'RESUMEN GENERAL DE INSPECCIONES DIARIAS'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_resumen_gral_insp_diarias"

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_codexport,integer(istr_mant.argumento[2]), &
                             date(istr_mant.argumento[5]),date(istr_mant.argumento[6]))
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fecha_desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.Modify("fecha_hasta.text = '" + em_fechahasta.text + "'")
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_gral_insp_diaria
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2359
integer y = 756
integer taborder = 50
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_resumen_gral_insp_diaria
integer x = 297
integer y = 716
integer width = 1934
integer height = 288
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumen_gral_insp_diaria
integer x = 430
integer y = 540
integer width = 210
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_resumen_gral_insp_diaria
integer x = 736
integer y = 524
integer width = 960
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer   li_nula

iuo_plantadesp    = Create    uo_plantadesp

IF NOT iuo_plantadesp.existefrigo(Integer(data),True,sqlca) THEN
	This.SetItem(1, "plde_codigo", Long(li_nula))
	RETURN 1
ELSE
	istr_mant.argumento[2]=String(data)
END IF	

end event

event itemerror;RETURN 1
end event

type em_fechadesde from editmask within w_info_resumen_gral_insp_diaria
integer x = 736
integer y = 796
integer width = 402
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_Mant.Argumento[5] = This.Text
end event

type st_44 from statictext within w_info_resumen_gral_insp_diaria
integer x = 297
integer y = 424
integer width = 1934
integer height = 288
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_resumen_gral_insp_diaria
integer x = 430
integer y = 812
integer width = 210
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_resumen_gral_insp_diaria
integer x = 1289
integer y = 812
integer width = 210
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_resumen_gral_insp_diaria
integer x = 1586
integer y = 796
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_Mant.Argumento[6] = This.Text
end event

