$PBExportHeader$w_mant_facturacion_guias.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_facturacion_guias from w_mant_directo
end type
type st_2 from statictext within w_mant_facturacion_guias
end type
type em_desde from editmask within w_mant_facturacion_guias
end type
type st_3 from statictext within w_mant_facturacion_guias
end type
type dw_4 from datawindow within w_mant_facturacion_guias
end type
type em_hasta from editmask within w_mant_facturacion_guias
end type
type st_1 from statictext within w_mant_facturacion_guias
end type
type dw_3 from datawindow within w_mant_facturacion_guias
end type
type st_4 from statictext within w_mant_facturacion_guias
end type
end forward

global type w_mant_facturacion_guias from w_mant_directo
integer x = 155
integer y = 156
integer width = 3685
integer height = 1880
string title = "MANTENEDOR DE FACTURAS EN DESPACHOS"
event ue_validaborrar ( )
st_2 st_2
em_desde em_desde
st_3 st_3
dw_4 dw_4
em_hasta em_hasta
st_1 st_1
dw_3 dw_3
st_4 st_4
end type
global w_mant_facturacion_guias w_mant_facturacion_guias

type variables
DataWindowChild	idwc_tipopro, idwc_transportista, idwc_planta
Integer ii_pallet, ii_nuevo

//str_busqueda istr_busq
//str_mant istr_mant
Integer	ii_tipo, ii_contador
String	is_report

uo_plantadesp			iuo_Planta




end variables

forward prototypes
public function boolean existe_transportista (integer ai_transporte)
end prototypes

public function boolean existe_transportista (integer ai_transporte);Integer	li_Existes
Boolean	lb_Retorno

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.transportista
WHERE  tran_codigo	=	:ai_transporte;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de transportista")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Transportista no Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF

Return lb_retorno

end function

event open;date	ld_fecha, ld_actual

x				= 0
y				= 0
//This.Width	= dw_1.width + 540
//This.Height	= 1993
im_menu	= m_principal

ld_actual	=	Today()
ld_fecha	=	RelativeDate(ld_actual, -365)

em_desde.Text	=	String(ld_fecha)
em_hasta.Text	=	String(ld_actual)

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
buscar			= "Código:Ncodigo,Descripción:Sconcepto"
ordenar			= "Código:codigo,Descripción:concepto"
is_ultimacol	= "columna"

ii_tipo	=	Integer(Message.StringParm)

dw_3.SetTransObject(sqlca)
dw_3.GetChild("clie_codigo", idwc_transportista)
idwc_transportista.SetTransObject(SQLCA)
idwc_transportista.Retrieve()
dw_3.InsertRow(0)

dw_4.SetTransObject(sqlca)
dw_4.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_4.InsertRow(0)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)


istr_mant.argumento[2] =String(gi_codplanta)


iuo_Planta				=	Create uo_plantadesp

end event

on w_mant_facturacion_guias.create
int iCurrent
call super::create
this.st_2=create st_2
this.em_desde=create em_desde
this.st_3=create st_3
this.dw_4=create dw_4
this.em_hasta=create em_hasta
this.st_1=create st_1
this.dw_3=create dw_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.em_desde
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.dw_4
this.Control[iCurrent+5]=this.em_hasta
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.dw_3
this.Control[iCurrent+8]=this.st_4
end on

on w_mant_facturacion_guias.destroy
call super::destroy
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_3)
destroy(this.dw_4)
destroy(this.em_hasta)
destroy(this.st_1)
destroy(this.dw_3)
destroy(this.st_4)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
Date	ld_desde, ld_hasta

ld_desde = Date(em_desde.Text)
ld_hasta = Date(em_hasta.Text)

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]),ld_desde,ld_hasta)
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF














end event

event ue_imprimir;Date	ld_desde, ld_hasta
SetPointer(HourGlass!)

istr_mant.argumento[5] = string(em_desde.Text)
istr_mant.argumento[6] = String(em_hasta.Text)

OpenWithParm(w_info_factura_guias, istr_mant)



end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF

	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
		ls_colu[li_cont]	= "pafr_prdrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
		ls_colu[li_cont]	= "pafr_calrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
		ls_colu[li_cont]	= "pafr_huert4"
	END IF	

	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
		ls_colu[li_cont]	= "pafr_cuart4"
	END IF	

	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
		ls_colu[li_cont]	= "pafr_rotpak"
	END IF	
	
	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de fecha embalaje Rotulada"
		ls_colu[li_cont]	= "pafr_fecrot"
	END IF		
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
//		ii_contador = 1
//		
//		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
//			ii_contador++
//		loop 
//		ii_contador --
//		dw_1.SetRow(ii_contador)
//		Message.DoubleParm = -1
	END IF
END IF





end event

type st_encabe from w_mant_directo`st_encabe within w_mant_facturacion_guias
integer x = 87
integer y = 60
integer width = 3118
integer height = 356
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_facturacion_guias
integer x = 3328
integer y = 428
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_3.Enabled			=	TRUE
dw_4.Enabled			=	TRUE

dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetFocus()
dw_3.Object.tran_codigo.Background.Color=	RGB(255, 255, 255)


dw_4.Reset()
dw_4.InsertRow(0)
dw_4.Object.plde_codigo.Background.Color=	RGB(255, 255, 255)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

pb_grabar.Enabled 	= False
pb_imprimir.Enabled 	= False

istr_mant.argumento[2] =String(gi_codplanta)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_facturacion_guias
integer x = 3323
integer y = 136
integer taborder = 50
end type

event pb_lectura::clicked;integer li_nombrevari


IF IsNull(dw_4.Object.plde_codigo[1]) OR dw_4.Object.plde_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Planta Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_3.Object.tran_codigo[1]) OR  dw_3.Object.tran_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Transportista Previamente",Exclamation!)
	RETURN
ELSE
	dw_3.Enabled	=	FALSE
	dw_4.Enabled	=	FALSE
	dw_4.Object.plde_codigo.Background.Color=	RGB(166,180,210)
	dw_3.Object.tran_codigo.Background.Color=	RGB(166,180,210)
	Parent.PostEvent("ue_recuperadatos")
	
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_facturacion_guias
boolean visible = false
integer x = 3323
integer y = 788
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_facturacion_guias
boolean visible = false
integer x = 3323
integer y = 608
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_facturacion_guias
integer x = 3310
integer y = 1492
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_facturacion_guias
integer x = 3319
integer y = 1152
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_facturacion_guias
integer x = 3323
integer y = 972
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_facturacion_guias
integer x = 87
integer y = 436
integer width = 3118
integer height = 1288
integer taborder = 60
string dataobject = "dw_mues_factura_guias"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type st_2 from statictext within w_mant_facturacion_guias
integer x = 219
integer y = 160
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type em_desde from editmask within w_mant_facturacion_guias
integer x = 631
integer y = 276
integer width = 480
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;//istr_mant.argumento[6]=em_numero.text

//IF existepallet(Long(em_numero.text)) = False THEN
//	This.SetFocus()
//END IF
end event

type st_3 from statictext within w_mant_facturacion_guias
integer x = 219
integer y = 296
integer width = 389
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_4 from datawindow within w_mant_facturacion_guias
integer x = 622
integer y = 140
integer width = 965
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null


SetNull(ll_null)

IF Not iuo_Planta.Existe(Integer(Data), True, sqlca) THEN
	This.SetItem(1, "plde_codigo", Integer(ll_null))

	RETURN 1
ELSE			
	istr_mant.argumento[2]=String(data)
END IF		



end event

event itemerror;Return 1
end event

type em_hasta from editmask within w_mant_facturacion_guias
integer x = 2176
integer y = 276
integer width = 480
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;//istr_mant.argumento[6]=em_numero.text
//
//IF existepallet(Long(em_numero.text)) = False THEN
//	This.SetFocus()
//END IF
end event

type st_1 from statictext within w_mant_facturacion_guias
integer x = 1673
integer y = 292
integer width = 389
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_facturacion_guias
integer x = 2176
integer y = 140
integer width = 965
integer height = 96
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_transportista"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer 	ll_null


SetNull(ll_null)

IF existe_transportista(Integer(Data)) THEN
	This.SetItem(1, "tran_codigo", Integer(ll_null))

	RETURN 1
ELSE			
	istr_mant.argumento[1]=String(data)
END IF		



end event

type st_4 from statictext within w_mant_facturacion_guias
integer x = 1673
integer y = 156
integer width = 457
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Transportista"
boolean focusrectangle = false
end type

