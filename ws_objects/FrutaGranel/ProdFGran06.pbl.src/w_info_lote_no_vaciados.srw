$PBExportHeader$w_info_lote_no_vaciados.srw
$PBExportComments$Lotes no Vaciados
forward
global type w_info_lote_no_vaciados from w_para_informes
end type
type em_fecha from editmask within w_info_lote_no_vaciados
end type
type st_fecha from statictext within w_info_lote_no_vaciados
end type
type st_6 from statictext within w_info_lote_no_vaciados
end type
type st_3 from statictext within w_info_lote_no_vaciados
end type
type uo_selplanta from uo_seleccion_plantas within w_info_lote_no_vaciados
end type
end forward

global type w_info_lote_no_vaciados from w_para_informes
integer width = 2267
integer height = 1248
string title = "Informe Contratista"
em_fecha em_fecha
st_fecha st_fecha
st_6 st_6
st_3 st_3
uo_selplanta uo_selplanta
end type
global w_info_lote_no_vaciados w_info_lote_no_vaciados

type variables
DataWindowChild	idwc_Planta, idwc_linea, idwc_variedad, idwc_especie,idwc_cliente,idwc_productor,idwc_progproc,idwc_contratista
uo_plantadesp		iuo_Planta
uo_especie			iuo_Especie
uo_Variedades		iuo_Variedad

Integer    			ii_TipoOrden,ii_cliente, ii_planta, ii_especie, il_ppre_numero, ii_linea
Long 					il_programa
end variables

on w_info_lote_no_vaciados.create
int iCurrent
call super::create
this.em_fecha=create em_fecha
this.st_fecha=create st_fecha
this.st_6=create st_6
this.st_3=create st_3
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fecha
this.Control[iCurrent+2]=this.st_fecha
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.uo_selplanta
end on

on w_info_lote_no_vaciados.destroy
call super::destroy
destroy(this.em_fecha)
destroy(this.st_fecha)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.uo_selplanta)
end on

event open;Boolean	lb_Cerrar = False

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_paramplanta.CodigoPlanta)

	em_fecha.text	=	String(Date(Today()),'dd/mm/yyyy')
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_lote_no_vaciados
end type

type st_computador from w_para_informes`st_computador within w_info_lote_no_vaciados
end type

type st_usuario from w_para_informes`st_usuario within w_info_lote_no_vaciados
end type

type st_temporada from w_para_informes`st_temporada within w_info_lote_no_vaciados
end type

type p_logo from w_para_informes`p_logo within w_info_lote_no_vaciados
end type

type st_titulo from w_para_informes`st_titulo within w_info_lote_no_vaciados
integer y = 316
integer width = 1554
string text = "Informe Lotes no Vaciados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_lote_no_vaciados
integer x = 1874
integer y = 296
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila

istr_info.Titulo		=	"Informe Lotes no Vaciados"
istr_info.Copias		=	1

OpenWithParm(vinf, istr_info)
Vinf.dw_1.DataObject =	"dw_info_lotesnovaciados"
Vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, DateTime(em_Fecha.Text))
 
IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	ParamTemporada(gstr_paramtempo)
	vinf.dw_1.Modify("nombre_temporada.text = '" + gstr_paramtempo.nombre + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_lote_no_vaciados
integer x = 1874
integer y = 560
integer taborder = 90
end type

type em_fecha from editmask within w_info_lote_no_vaciados
integer x = 713
integer y = 704
integer width = 485
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_fecha from statictext within w_info_lote_no_vaciados
integer x = 306
integer y = 708
integer width = 347
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_lote_no_vaciados
integer x = 306
integer y = 544
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_lote_no_vaciados
integer x = 242
integer y = 428
integer width = 1554
integer height = 420
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_info_lote_no_vaciados
event destroy ( )
integer x = 713
integer y = 536
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

