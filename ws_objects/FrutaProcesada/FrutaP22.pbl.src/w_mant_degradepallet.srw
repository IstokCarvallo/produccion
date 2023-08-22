$PBExportHeader$w_mant_degradepallet.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_degradepallet from w_mant_directo
end type
type st_2 from statictext within w_mant_degradepallet
end type
type st_1 from statictext within w_mant_degradepallet
end type
type em_numero from editmask within w_mant_degradepallet
end type
type st_3 from statictext within w_mant_degradepallet
end type
type dw_2 from datawindow within w_mant_degradepallet
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_degradepallet
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_degradepallet
end type
end forward

global type w_mant_degradepallet from w_mant_directo
integer x = 155
integer y = 156
integer width = 3342
integer height = 1892
string title = "MANTENEDOR DEGRADACION PALLET"
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
em_numero em_numero
st_3 st_3
dw_2 dw_2
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_mant_degradepallet w_mant_degradepallet

type variables
DataWindowChild	idwc_tipopro
Integer 	ii_pallet, ii_nuevo

Integer	ii_tipo, ii_contador
String		is_report

uo_calibre				iuo_calibre



end variables

forward prototypes
public function boolean existepallet (long al_numero)
public function boolean existeembalaje (string as_embalaje, integer al_cliente)
end prototypes

public function boolean existepallet (long al_numero);Integer	li_codexp, li_Tipova
Date		ld_fecha
String		ls_Embarque
Long		ll_pallet

SELECT paen_numero
	INTO	:ll_pallet
	FROM	dbo.palletencab
	WHERE	plde_codigo =	:uo_SelPlanta.Codigo
	AND	clie_codigo	=	:uo_SelCliente.Codigo
	AND	paen_numero	=	:al_numero
	AND	paen_Estado =	1;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
	em_numero.Text = ''
	em_numero.SetFocus()
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Número de Pallet Indicado.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	em_numero.Text = ''
	em_numero.SetFocus()
	RETURN False
END IF

RETURN TRUE
	
end function

public function boolean existeembalaje (string as_embalaje, integer al_cliente);Integer	li_Existes
Boolean	lb_Retorno


SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.embalajesprod
WHERE  emba_codigo =	:as_embalaje
AND	 clie_codigo  = :al_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de embalajesprod")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Embalaje no Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	dw_2.SetTransObject(sqlca)
	ii_tipo	=	Integer(Message.StringParm)
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	
	uo_SelPlanta.Filtra(1)
	
	uo_SelCliente.Inicia(gi_codexport)
	uo_SelPlanta.Inicia(gi_codplanta)

	iuo_calibre   			=	Create uo_calibre
	
	buscar			= "Código:Ncodigo,Descripción:Sconcepto"
	ordenar			= "Código:codigo,Descripción:concepto"
End If
end event

on w_mant_degradepallet.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_numero=create em_numero
this.st_3=create st_3
this.dw_2=create dw_2
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_numero
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.uo_selcliente
this.Control[iCurrent+7]=this.uo_selplanta
end on

on w_mant_degradepallet.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.dw_2)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_2.Retrieve(long(em_Numero.Text),uo_SelCliente.Codigo,uo_SelPlanta.Codigo)
	dw_1.Retrieve(long(em_Numero.Text),uo_SelCliente.Codigo,uo_SelPlanta.Codigo)
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
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

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

lstr_info.titulo	= "DETALLE DE PALLET"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_rotulacion_pallet"
vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(long(em_numero.Text), uo_SelCliente.Codigo, uo_SelPlanta.Codigo)

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
END IF

SetPointer(Arrow!)
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

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_embalaje, ls_variedad, ls_calibre, ls_especie
Long		ll_Fila, ll_fila1, ll_fila2

FOR ll_fila1 = 1 TO dw_2.RowCount()
	FOR ll_fila2 = 1 TO dw_1.RowCount() 
		ls_variedad = string(dw_2.Object.vari_codigo[ll_fila1])
		ls_embalaje = string(dw_2.Object.emba_codigo[ll_fila1])
		ls_calibre 	= string(dw_2.Object.pafr_calibr[ll_fila1])		
		ls_especie 	= string(dw_2.Object.espe_codigo[ll_fila1])		
	
		ll_fila	= dw_1.Find("vari_codigo = " + ls_variedad + " AND espe_codigo = " + ls_especie + &
								 " AND emba_codigo = '" + ls_embalaje + "'" + &
								 " AND pafr_calibr = '" + ls_calibre + "'", ll_fila2, dw_1.RowCount())

		IF ll_fila > 0 THEN
			IF dw_2.Object.embalaje[ll_fila1] <> '' THEN
				dw_1.Object.emba_codigo[ll_fila] = dw_2.Object.embalaje[ll_fila1]
				dw_1.Object.pafr_embrea[ll_Fila]	= dw_1.Object.emba_codigo[ll_Fila]
			END IF
			
			IF dw_2.Object.calibre[ll_fila1] <> '' THEN
				ls_calibre = dw_2.Object.calibre[ll_fila1]
				
				dw_1.Object.pafr_calibr[ll_fila] = iuo_calibre.fomato_calibre(ls_calibre,sqlca)
				dw_1.Object.pafr_calrot[ll_Fila]	= dw_1.Object.pafr_calibr[ll_Fila]
			END IF
			dw_1.Object.pafr_cuart3[ll_Fila]	= 99
		END IF									 							 
	NEXT
NEXT
end event

event resize;call super::resize;dw_2.Height = dw_1.Height 
dw_2.Width = dw_1.Width
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_degradepallet
integer x = 87
integer y = 60
integer width = 2743
integer height = 416
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_degradepallet
integer x = 2953
integer y = 428
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;em_numero.Enabled	=	TRUE

em_numero.BackColor=	RGB(255, 255, 255)
em_numero.text = ""

dw_2.Reset()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_degradepallet
integer x = 2944
integer taborder = 40
end type

event pb_lectura::clicked;integer li_nombrevari

IF IsNull(integer(em_numero.text)) OR integer(em_numero.text) = 0 THEN
	MessageBox("Atención","Debe Seleccionar Nº de Pallet Previamente",Exclamation!)
	RETURN
	em_numero.SetFocus()
ELSE
	em_numero.BackColor=	553648127
	em_numero.TextColor=	RGB(255,255,255)
	Parent.PostEvent("ue_recuperadatos")
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_degradepallet
boolean visible = false
integer x = 2949
integer y = 788
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_degradepallet
boolean visible = false
integer x = 2949
integer y = 608
integer taborder = 70
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_degradepallet
integer x = 2949
integer y = 1532
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_degradepallet
integer x = 2949
integer y = 1148
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_degradepallet
integer x = 2949
integer y = 968
integer taborder = 90
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_degradepallet
boolean visible = false
integer x = 87
integer y = 488
integer width = 2743
integer height = 1264
integer taborder = 50
string dataobject = "dw_mues_degradepallet"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

	
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF

IF dw_1.rowcount() > 0 THEN
	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
		dw_1.SetRow(ii_contador)
	END IF
END IF



//CHOOSE CASE ls_Columna	
//		
//	CASE "pafr_varrot"
//		istr_mant.Argumento[2]	=	Data	
//			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
//			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
//			IF NoExisteVariedad(ls_Columna,Data)	THEN
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//	
//END CHOOSE
//
//
//
//
//
end event

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

type st_2 from statictext within w_mant_degradepallet
integer x = 690
integer y = 228
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_degradepallet
integer x = 690
integer y = 112
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_degradepallet
integer x = 1029
integer y = 328
integer width = 402
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;If IsNull(This.Text) Or This.Text = '' Then Return
If Not ExistePallet(Long(This.text)) Then This.SetFocus()
end event

type st_3 from statictext within w_mant_degradepallet
integer x = 690
integer y = 344
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_degradepallet
integer x = 87
integer y = 492
integer width = 2743
integer height = 1188
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_degradepallet_agrupado"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String ls_Columna, ls_null, ls_varnom, ls_calibre

SetNull(ls_null)

dw_2.AcceptText()
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case 'embalaje'
		If ExisteEmbalaje(Data,uo_SelCliente.Codigo)	Then
			dw_2.SetItem(il_Fila,'embalaje',ls_Null)
			Return 1
		End If
		
	Case "calibre"		
		If NOT iuo_calibre.existe(dw_2.Object.espe_codigo[row],dw_2.Object.vari_codigo[row]	,data,True,sqlca) Then
			dw_2.SetItem(il_fila, "calibre", ls_Null)
			Return 1
		End If	
End Choose




end event

event itemerror;Return 1
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_degradepallet
event destroy ( )
integer x = 1029
integer y = 96
integer height = 96
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_degradepallet
event destroy ( )
integer x = 1029
integer y = 212
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

