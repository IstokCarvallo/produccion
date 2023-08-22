$PBExportHeader$w_info_spro_repalasgenca.srw
forward
global type w_info_spro_repalasgenca from w_para_informes
end type
type st_1 from statictext within w_info_spro_repalasgenca
end type
type st_3 from statictext within w_info_spro_repalasgenca
end type
type dw_planta from datawindow within w_info_spro_repalasgenca
end type
type em_hasta from editmask within w_info_spro_repalasgenca
end type
type st_4 from statictext within w_info_spro_repalasgenca
end type
type em_desde from editmask within w_info_spro_repalasgenca
end type
type st_22 from statictext within w_info_spro_repalasgenca
end type
type st_5 from statictext within w_info_spro_repalasgenca
end type
type cbx_movto from checkbox within w_info_spro_repalasgenca
end type
type em_movtod from editmask within w_info_spro_repalasgenca
end type
type em_movtoh from editmask within w_info_spro_repalasgenca
end type
type st_2 from statictext within w_info_spro_repalasgenca
end type
type dw_cliente from datawindow within w_info_spro_repalasgenca
end type
type st_6 from statictext within w_info_spro_repalasgenca
end type
type dw_especie from datawindow within w_info_spro_repalasgenca
end type
type cbx_especie from checkbox within w_info_spro_repalasgenca
end type
type cbx_variedad from checkbox within w_info_spro_repalasgenca
end type
type sle_variedad from singlelineedit within w_info_spro_repalasgenca
end type
type cb_buscavariedad from commandbutton within w_info_spro_repalasgenca
end type
type em_variedad from editmask within w_info_spro_repalasgenca
end type
type st_variedad from statictext within w_info_spro_repalasgenca
end type
type cbx_emba from checkbox within w_info_spro_repalasgenca
end type
type cb_buscaembalaje from commandbutton within w_info_spro_repalasgenca
end type
type em_embalaje from editmask within w_info_spro_repalasgenca
end type
type st_7 from statictext within w_info_spro_repalasgenca
end type
type st_10 from statictext within w_info_spro_repalasgenca
end type
type em_calidad from editmask within w_info_spro_repalasgenca
end type
type cbx_calibre from checkbox within w_info_spro_repalasgenca
end type
end forward

global type w_info_spro_repalasgenca from w_para_informes
integer width = 2592
integer height = 1784
boolean maxbox = false
boolean resizable = false
st_1 st_1
st_3 st_3
dw_planta dw_planta
em_hasta em_hasta
st_4 st_4
em_desde em_desde
st_22 st_22
st_5 st_5
cbx_movto cbx_movto
em_movtod em_movtod
em_movtoh em_movtoh
st_2 st_2
dw_cliente dw_cliente
st_6 st_6
dw_especie dw_especie
cbx_especie cbx_especie
cbx_variedad cbx_variedad
sle_variedad sle_variedad
cb_buscavariedad cb_buscavariedad
em_variedad em_variedad
st_variedad st_variedad
cbx_emba cbx_emba
cb_buscaembalaje cb_buscaembalaje
em_embalaje em_embalaje
st_7 st_7
st_10 st_10
em_calidad em_calidad
cbx_calibre cbx_calibre
end type
global w_info_spro_repalasgenca w_info_spro_repalasgenca

type variables
str_mant  istr_mant
datawindowchild idwc_planta, idwc_cliente,idwc_especie

string is_TipoInf

uo_calibre	iuo_calibre
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existeproductor (long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_spro_repalasgenca.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.dw_planta=create dw_planta
this.em_hasta=create em_hasta
this.st_4=create st_4
this.em_desde=create em_desde
this.st_22=create st_22
this.st_5=create st_5
this.cbx_movto=create cbx_movto
this.em_movtod=create em_movtod
this.em_movtoh=create em_movtoh
this.st_2=create st_2
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_especie=create dw_especie
this.cbx_especie=create cbx_especie
this.cbx_variedad=create cbx_variedad
this.sle_variedad=create sle_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.em_variedad=create em_variedad
this.st_variedad=create st_variedad
this.cbx_emba=create cbx_emba
this.cb_buscaembalaje=create cb_buscaembalaje
this.em_embalaje=create em_embalaje
this.st_7=create st_7
this.st_10=create st_10
this.em_calidad=create em_calidad
this.cbx_calibre=create cbx_calibre
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.dw_planta
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.st_22
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.cbx_movto
this.Control[iCurrent+10]=this.em_movtod
this.Control[iCurrent+11]=this.em_movtoh
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.dw_cliente
this.Control[iCurrent+14]=this.st_6
this.Control[iCurrent+15]=this.dw_especie
this.Control[iCurrent+16]=this.cbx_especie
this.Control[iCurrent+17]=this.cbx_variedad
this.Control[iCurrent+18]=this.sle_variedad
this.Control[iCurrent+19]=this.cb_buscavariedad
this.Control[iCurrent+20]=this.em_variedad
this.Control[iCurrent+21]=this.st_variedad
this.Control[iCurrent+22]=this.cbx_emba
this.Control[iCurrent+23]=this.cb_buscaembalaje
this.Control[iCurrent+24]=this.em_embalaje
this.Control[iCurrent+25]=this.st_7
this.Control[iCurrent+26]=this.st_10
this.Control[iCurrent+27]=this.em_calidad
this.Control[iCurrent+28]=this.cbx_calibre
end on

on w_info_spro_repalasgenca.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.dw_planta)
destroy(this.em_hasta)
destroy(this.st_4)
destroy(this.em_desde)
destroy(this.st_22)
destroy(this.st_5)
destroy(this.cbx_movto)
destroy(this.em_movtod)
destroy(this.em_movtoh)
destroy(this.st_2)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_especie)
destroy(this.cbx_especie)
destroy(this.cbx_variedad)
destroy(this.sle_variedad)
destroy(this.cb_buscavariedad)
destroy(this.em_variedad)
destroy(this.st_variedad)
destroy(this.cbx_emba)
destroy(this.cb_buscaembalaje)
destroy(this.em_embalaje)
destroy(this.st_7)
destroy(this.st_10)
destroy(this.em_calidad)
destroy(this.cbx_calibre)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

is_TipoInf = message.StringParm

IF is_TipoInf = "2" THEN
	st_titulo.text = "Informe Saldo de Cajas Por Pallet"
ELSE	
	st_1.height   = 568
	this.height   = 1084
	st_6.visible = false
	cbx_especie.visible = false
	cbx_variedad.visible = false
	cbx_emba.visible = false
	cbx_calibre.visible = false
	dw_especie.visible = false
END IF

//PLANTA//
dw_Planta.Getchild("plde_codigo",idwc_planta)
idwc_planta.SettransObject(sqlca)
idwc_planta.Retrieve()
dw_Planta.InsertRow(0)

dw_Planta.SetItem(1,"plde_codigo",gi_CodPlanta)

dw_cliente.Getchild("clie_codigo",idwc_cliente)
idwc_cliente.SettransObject(sqlca)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)

dw_cliente.SetItem(1,"clie_codigo",gi_CodExport)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
//dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

iuo_calibre   						=	Create uo_calibre

end event

type st_computador from w_para_informes`st_computador within w_info_spro_repalasgenca
end type

type st_usuario from w_para_informes`st_usuario within w_info_spro_repalasgenca
end type

type st_temporada from w_para_informes`st_temporada within w_info_spro_repalasgenca
end type

type p_logo from w_para_informes`p_logo within w_info_spro_repalasgenca
end type

type st_titulo from w_para_informes`st_titulo within w_info_spro_repalasgenca
integer width = 1833
string text = "Informe  Repalletizaje"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_spro_repalasgenca
integer x = 2231
integer y = 1072
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila, ll_desde, ll_hasta
String	ls_FechaDesde, ls_fecha, ls_FechaHasta, ls_embalaje, ls_calibre
Date ldt_Fechadesde, ldt_FechaHasta, ldt_fechaprueba
integer li_especie, li_variedad


istr_info.titulo	= "REPALLETIZAJE"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

IF is_tipoinf = "1" THEN
	vinf.dw_1.DataObject = "dw_info_repalasgenca"
ELSE
	vinf.dw_1.DataObject = "dw_info_saldo_cajas_repa"
	
	// Especie
	If cbx_especie.checked THEN
		li_especie = -1
	ELSE
		li_especie = dw_especie.object.espe_codigo[1]
		IF isnull(li_especie) or li_especie = 0  THEN
			MessageBox( "ATENCIÓN", "Falta ingreso de especie.", &
							 StopSign!, Ok!)
			RETURN 1
		END IF
	END IF
	
	// variedad
	If cbx_variedad.checked THEN
		li_variedad = -1
	ELSE
		li_variedad = integer(em_variedad.text)
		IF isnull(li_variedad) or li_variedad = 0  THEN
			MessageBox( "ATENCIÓN", "Falta ingreso de variedad.", &
							 StopSign!, Ok!)
			RETURN 1
		END IF
	END IF
	
	// Embalaje
	If cbx_emba.checked THEN
		ls_embalaje = "*"
	ELSE
		ls_embalaje = em_embalaje.text
		IF isnull(ls_embalaje) or ls_embalaje =""  THEN
			MessageBox( "ATENCIÓN", "Falta ingreso de embalaje.", &
							 StopSign!, Ok!)
			RETURN 1
		END IF
	END IF

	// Calibre
	If cbx_calibre.checked THEN
		ls_calibre = "*"
	ELSE
		ls_calibre = em_calidad.text
		IF isnull(ls_calibre) or ls_calibre =""  THEN
			MessageBox( "ATENCIÓN", "Falta ingreso de calibre.", &
							 StopSign!, Ok!)
			RETURN 1
		END IF
	END IF

	
	
	
END IF	

vinf.dw_1.SetTransObject(sqlca)

If cbx_movto.Checked Then
	ll_desde	=	0
	ll_hasta  =  0
	//ll_hasta	=	99999999
Else
	ll_desde	=	Long(em_movtod.Text)
	ll_hasta	=	Long(em_movtoh.Text)	
End IF

ldt_fechadesde	=	Date(em_desde.Text)
ls_FechaDesde	=	String(ldt_fechadesde, 'dd/mm/yyyy')

If cbx_movto.checked THEN
	If IsNull(ldt_fechadesde) or ldt_fechadesde<= ldt_fechaprueba Then
		MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
						 StopSign!, Ok!)
		RETURN 1				 
	END If
END If

ls_fecha			=	em_hasta.Text
ldt_fechahasta = 	Date(ls_fecha)
ls_FechaHasta	=	String(ldt_fechahasta, 'dd/mm/yyyy')

If cbx_movto.checked THEN
	If IsNull(ldt_fechahasta) or ldt_fechahasta<=ldt_fechaprueba Then
		MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
						 StopSign!, Ok!)
		RETURN 1				 
	END If
END If

IF is_tipoinf = "1" THEN
	fila = vinf.dw_1.Retrieve(dw_cliente.Object.clie_codigo[1],dw_planta.Object.plde_codigo[1], ldt_fechadesde, &
									ldt_fechahasta, ll_desde, ll_hasta)
ELSE
	fila = vinf.dw_1.Retrieve(dw_cliente.Object.clie_codigo[1],dw_planta.Object.plde_codigo[1], ldt_fechadesde, &
									ldt_fechahasta, ll_desde, ll_hasta,li_especie, li_variedad, ls_embalaje, ls_calibre)
END IF	

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_spro_repalasgenca
integer x = 2231
integer y = 1368
integer taborder = 80
end type

type st_1 from statictext within w_info_spro_repalasgenca
integer x = 247
integer y = 440
integer width = 1833
integer height = 1060
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 644
integer width = 352
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_spro_repalasgenca
integer x = 658
integer y = 624
integer width = 1147
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

type em_hasta from editmask within w_info_spro_repalasgenca
integer x = 1408
integer y = 872
integer width = 352
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

type st_4 from statictext within w_info_spro_repalasgenca
integer x = 1111
integer y = 880
integer width = 210
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_spro_repalasgenca
integer x = 658
integer y = 872
integer width = 352
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

type st_22 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 880
integer width = 352
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 760
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Nº Repalet."
boolean focusrectangle = false
end type

type cbx_movto from checkbox within w_info_spro_repalasgenca
integer x = 1509
integer y = 752
integer width = 311
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todas"
end type

event clicked;If This.Checked Then
	em_movtod.Enabled	=	False
	em_movtoh.Enabled	=	False
	em_desde.Enabled	=	True
	em_hasta.Enabled	=	True
	em_movtod.Text		=	''
	em_movtoh.Text		=	''
Else
	em_movtod.Enabled	=	True
	em_movtoh.Enabled	=	True
	em_desde.Enabled	=	False
	em_hasta.Enabled	=	False
	em_movtod.Text		=	''
	em_movtoh.Text		=	''
End IF
end event

type em_movtod from editmask within w_info_spro_repalasgenca
integer x = 658
integer y = 744
integer width = 352
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long	ll_desde, ll_hasta

ll_desde	=	Long(This.Text)
ll_hasta	=	Long(em_movtoh.Text)

If ll_hasta > 0 Then 
	If ll_desde > ll_hasta Then
		MessageBox('Alerta...', 'El movimento Inicial no puede ser mayor al moviemnto Final.')
		This.Text	=	''
		This.SetFocus()
		Return
	End If
End IF
end event

type em_movtoh from editmask within w_info_spro_repalasgenca
integer x = 1111
integer y = 744
integer width = 352
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long	ll_desde, ll_hasta

ll_desde	=	Long(em_movtod.Text)
ll_hasta	=	Long(This.Text)

If ll_desde > 0 Then 
	If ll_hasta < ll_desde Then
		MessageBox('Alerta...', 'El movimento Final no puede ser menor al moviemnto Inicial.')
		This.Text	=	''
		This.SetFocus()
		Return
	End If
End If 
end event

type st_2 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 524
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_spro_repalasgenca
integer x = 658
integer y = 504
integer width = 1138
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

type st_6 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 1008
integer width = 421
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_spro_repalasgenca
integer x = 658
integer y = 996
integer width = 878
integer height = 100
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExisteEspecie(Integer(data)) THEN
	istr_mant.argumento[4]	=	data
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	istr_mant.argumento[4]	=	String(gi_CodEspecie)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_especie from checkbox within w_info_spro_repalasgenca
integer x = 1769
integer y = 1012
integer width = 270
integer height = 76
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;integer li_null

SetNull(li_null)

IF This.Checked THEN
	dw_especie.Enabled		=	False
	dw_especie.SetItem(1, "espe_codigo", li_null)
	cbx_variedad.checked = true
	cbx_variedad.TriggerEvent("Clicked")
ELSE
	istr_mant.argumento[4]	=	'11'
	dw_especie.Enabled		=	True
	dw_especie.SetFocus()
END IF

end event

type cbx_variedad from checkbox within w_info_spro_repalasgenca
integer x = 1769
integer y = 1136
integer width = 270
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	istr_mant.argumento[5]		=	'-1'
	
ELSE
	IF cbx_especie.Checked = true THEN 
		this.Checked = true
		Return 1
	END IF	
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF
end event

type sle_variedad from singlelineedit within w_info_spro_repalasgenca
integer x = 1010
integer y = 1128
integer width = 741
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type cb_buscavariedad from commandbutton within w_info_spro_repalasgenca
integer x = 910
integer y = 1132
integer width = 96
integer height = 84
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

IF isnull(dw_especie.Object.espe_codigo[1]) or dw_especie.Object.espe_codigo[1] = 0 THEN
	MessageBox("ATENCIÓN","Debe ingresar una Especie")
	Return 1
END IF	

lstr_busq.argum[1]	=	string(dw_cliente.object.clie_codigo[1])      // Cliente
lstr_busq.argum[2]	=	string(dw_especie.Object.espe_codigo[1]) // Especie

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	em_variedad.SetFocus()
ELSE
	em_variedad.Text			=	lstr_busq.argum[4]
	sle_variedad.Text			=	lstr_busq.argum[5]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	
END IF

end event

type em_variedad from editmask within w_info_spro_repalasgenca
integer x = 658
integer y = 1124
integer width = 233
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Integer		li_especie, li_variedad
String		ls_Nombre

//li_especie	=	Integer(istr_mant.argumento[4]) // Especie
li_especie	=	dw_especie.Object.espe_codigo[1]
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dba.variedades
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variedades")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	sle_variedad.Text			=	ls_nombre
	istr_mant.argumento[5]	=	String(li_variedad)	
END IF
end event

type st_variedad from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 1140
integer width = 279
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_emba from checkbox within w_info_spro_repalasgenca
integer x = 1038
integer y = 1260
integer width = 270
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Text			=	''
	em_embalaje.Enabled		=	False
	istr_mant.argumento[6]	=	'*'
	cb_buscaembalaje.Enabled = False;
ELSE
	em_embalaje.Enabled		=	True
	em_embalaje.SetFocus()
	cb_buscaembalaje.Enabled = True;
END IF

end event

type cb_buscaembalaje from commandbutton within w_info_spro_repalasgenca
integer x = 910
integer y = 1256
integer width = 96
integer height = 84
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	string(dw_cliente.object.clie_codigo[1]) // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[6]	=	lstr_busq.argum[2]
END IF
end event

type em_embalaje from editmask within w_info_spro_repalasgenca
integer x = 658
integer y = 1248
integer width = 233
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	dw_cliente.object.clie_codigo[1] // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type st_7 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 1264
integer width = 288
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_spro_repalasgenca
integer x = 288
integer y = 1384
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_spro_repalasgenca
integer x = 658
integer y = 1368
integer width = 233
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	li_especie	=	dw_especie.Object.espe_codigo[1] // Especie
	//li_variedad	=	Integer(istr_mant.argumento[9]) // Variedad
	li_variedad	=	Integer(em_variedad.text)
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[7]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type cbx_calibre from checkbox within w_info_spro_repalasgenca
integer x = 1038
integer y = 1392
integer width = 270
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

