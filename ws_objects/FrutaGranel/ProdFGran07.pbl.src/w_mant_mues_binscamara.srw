$PBExportHeader$w_mant_mues_binscamara.srw
forward
global type w_mant_mues_binscamara from w_mant_directo
end type
type gb_4 from groupbox within w_mant_mues_binscamara
end type
type st_3 from statictext within w_mant_mues_binscamara
end type
type st_4 from statictext within w_mant_mues_binscamara
end type
type st_1 from statictext within w_mant_mues_binscamara
end type
type cbx_todos from checkbox within w_mant_mues_binscamara
end type
type rb_com from radiobutton within w_mant_mues_binscamara
end type
type rb_gra from radiobutton within w_mant_mues_binscamara
end type
type sle_tarjas from editmask within w_mant_mues_binscamara
end type
type rb_todas from radiobutton within w_mant_mues_binscamara
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_binscamara
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_binscamara
end type
end forward

global type w_mant_mues_binscamara from w_mant_directo
integer width = 3749
integer height = 2372
string title = "MANTENCION ESTIBA EN CAMARAS"
gb_4 gb_4
st_3 st_3
st_4 st_4
st_1 st_1
cbx_todos cbx_todos
rb_com rb_com
rb_gra rb_gra
sle_tarjas sle_tarjas
rb_todas rb_todas
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
end type
global w_mant_mues_binscamara w_mant_mues_binscamara

type variables
DataWindowChild		idwc_cliente, idwc_planta, idwc_camara

Integer			ii_orden, ii_estado
Long				il_tarja
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
public function boolean validacamara (integer valor, string columna)
public function boolean validaposicioncamara (integer valor, string columna)
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_Fila, ll_Pallet
Integer	li_Camara, li_Calle, li_Base, li_Posici

li_Camara	=	dw_1.Object.cama_codigo[il_fila]
li_Calle		= 	dw_1.Object.fgmb_calle[il_fila]
li_Base		= 	dw_1.Object.fgmb_base[il_fila]
li_Posici	= 	dw_1.Object.fgmb_posici[il_fila]

CHOOSE CASE columna
	CASE "fgmb_codigo"
		li_Camara	= Integer(valor)

	CASE "fgmb_calle"
		li_Calle		= Integer(valor)

	CASE "fgmb_base"
		li_Base		= Integer(valor)

	CASE "fgmb_posici"
		li_Posici	= Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("cama_codigo = " + String(li_Camara) + &
							" and " + "fgmb_calle = " + String(li_Calle) + &
							" and " + "fgmb_base = " + String(li_Base) + &
							" and " + "fgmb_posici = " + String(li_posici),1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Posición en Cámara Ya Fue Ingresada Anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean validacamara (integer valor, string columna);Long		ll_Fila, ll_Pallet
Integer	li_Camara, li_Calle, li_Base, li_Posici, li_cama, li_Calle2, li_Base2, li_Posici2


li_Camara	=	dw_1.Object.cama_codigo[il_fila]
li_Calle		= 	dw_1.Object.fgmb_calle[il_fila]
li_Base		= 	dw_1.Object.fgmb_base[il_fila]
li_Posici	= 	dw_1.Object.fgmb_posici[il_fila]

CHOOSE CASE columna
	CASE "cama_codigo"
		li_Camara	= Integer(valor)

	CASE "fgmb_calle"
		li_Calle		= Integer(valor)

	CASE "fgmb_base"
		li_Base		= Integer(valor)

	CASE "fgmb_posici"
		li_Posici	= Integer(valor)

END CHOOSE

SELECT cama_cancal, cama_canbas, cama_canpos
INTO :li_calle2, :li_base2, :li_posici2
FROM dbo.camarasbode
WHERE cama_codigo = :li_camara
AND   plde_codigo = :uo_SelPlanta.Codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CamarasBode")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox(	"Atención", "Código de Cámara no ha sido creado.~r~nIngrese otro código", &
									Exclamation!, Ok!)
	RETURN False
ELSE

	IF (li_Calle > li_Calle2) OR (li_Base > li_Base2) OR (li_Posici > li_Posici2) THEN
		MessageBox(	"Atención", "La posicion ingresada no es válida para esta Cámara.~r~nIngrese otro código", &
							Exclamation!, Ok!)
		RETURN False
	ELSE
		RETURN True
	END IF
END IF
end function

public function boolean validaposicioncamara (integer valor, string columna);Long		ll_Fila, ll_Pallet , ll_CuentaEnPosicion
Integer	li_Camara, li_Calle, li_Base, li_Posici, li_cama


li_Camara	=	dw_1.Object.cama_codigo[il_fila]
li_Calle		= 	dw_1.Object.fgmb_calle[il_fila]
li_Base		= 	dw_1.Object.fgmb_base[il_fila]
li_Posici	= 	dw_1.Object.fgmb_posici[il_fila]

CHOOSE CASE columna
	CASE "cama_codigo"
		li_Camara	= Integer(valor)

	CASE "fgmb_calle"
		li_Calle		= Integer(valor)

	CASE "fgmb_base"
		li_Base		= Integer(valor)

	CASE "fgmb_posici"
		li_Posici	= Integer(valor)

END CHOOSE

SELECT Count(fgmb_nrotar)
INTO :ll_CuentaEnPosicion
FROM dbo.spro_movtobins
WHERE cama_codigo = :li_Camara
AND   plde_codigo = :uo_SelPlanta.Codigo
AND   fgmb_calle  = :li_Calle
AND   fgmb_base   = :li_Base
AND   fgmb_posici = :li_Posici;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_Movtobins")
	RETURN False
ELSEIF ll_CuentaEnPosicion > 0 THEN
	RETURN False
ELSE
	RETURN True
END IF
end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ii_estado,il_tarja)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.GetChild("cama_codiog", idwc_camara)
		idwc_camara.SetTransObject(sqlca)
		idwc_camara.Retrieve(uo_SelPlanta.Codigo)
		dw_1.SetRow(1)
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	dw_1.GetChild("cama_codigo", idwc_camara)
	idwc_camara.SetTransObject(sqlca)
	idwc_camara.Retrieve(uo_SelPlanta.Codigo)
	
	istr_mant.dw				= dw_1
	
	buscar			= "Tarja Bins:fgmb_nrotar,Condición Bins:fgmb_estado,Camara:cama_codigo,Calle:fgmb_calle,Base:fgmb_base,Altura:fgmb_posici"
	ordenar			= "Tarja Bins:fgmb_nrotar,Condición Bins:fgmb_estado,Camara:cama_codigo,Calle:fgmb_calle,Base:fgmb_base,Altura:fgmb_posici"
End If
end event

on w_mant_mues_binscamara.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_3=create st_3
this.st_4=create st_4
this.st_1=create st_1
this.cbx_todos=create cbx_todos
this.rb_com=create rb_com
this.rb_gra=create rb_gra
this.sle_tarjas=create sle_tarjas
this.rb_todas=create rb_todas
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.cbx_todos
this.Control[iCurrent+6]=this.rb_com
this.Control[iCurrent+7]=this.rb_gra
this.Control[iCurrent+8]=this.sle_tarjas
this.Control[iCurrent+9]=this.rb_todas
this.Control[iCurrent+10]=this.uo_selplanta
this.Control[iCurrent+11]=this.uo_selcliente
end on

on w_mant_mues_binscamara.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.cbx_todos)
destroy(this.rb_com)
destroy(this.rb_gra)
destroy(this.sle_tarjas)
destroy(this.rb_todas)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
end on

event ue_nuevo;IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

dw_1.SetFocus()
dw_1.SetColumn(1)
end event

event ue_imprimir;String		ls_embalaje, ls_calibre, ls_data_object
str_info	lstr_info
Integer	fila,  li_camara, li_especie, li_variedad

SetPointer(HourGlass!)
OpenWithParm(vinf, istr_info)

li_camara					=	-1
li_especie					=	-1
li_variedad					=	-1

istr_info.titulo			= 'Informe Plano de Cámaras Por Bins'
vinf.dw_1.DataObject = "dw_info_estiba_camarasbins"
lstr_info.copias			= 1

vinf.dw_1.SetTransObject(sqlca)
fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, li_camara, li_especie, li_variedad)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_binscamara
integer x = 82
integer y = 20
integer width = 3063
integer height = 392
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_binscamara
integer x = 3287
integer y = 392
integer taborder = 110
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelPlanta.Bloquear(False)

sle_tarjas.Enabled		= 	True

sle_tarjas.Text 			=	'' 
il_fila						= 	0
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_binscamara
integer x = 3269
integer taborder = 50
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelPlanta.Bloquear(True)
sle_tarjas.Enabled		= False
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_binscamara
boolean visible = false
integer x = 3269
integer taborder = 130
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_binscamara
boolean visible = false
integer x = 3269
integer taborder = 120
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_binscamara
integer x = 3269
integer taborder = 160
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_binscamara
boolean visible = false
integer x = 3269
integer taborder = 150
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_binscamara
integer x = 3269
integer taborder = 140
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_binscamara
integer x = 82
integer y = 440
integer width = 3063
integer height = 1132
integer taborder = 100
string dataobject = "dw_mues_binscamara"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null
String	ls_Columna

SetNull(li_null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "cama_codigo"	
		IF NOT validacamara(Integer(data),ls_Columna) THEN
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1									
		END IF	
		
		IF NOT validaposicioncamara(Integer(data),ls_Columna) THEN
			MessageBox(	"Atención", "Posicion Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
		RETURN 1									
						
		END IF
		
		IF Duplicado(ls_Columna,data) THEN
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1											
		END IF
		
	CASE "fgmb_calle"
		IF NOT validacamara(Integer(data),ls_Columna) THEN
//			MessageBox(	"Atención", "Número de Calle No Existe para la Cámara Seleccionada.~r~nIngrese otro código", &
//											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1									
		END IF	

      IF NOT validaposicioncamara(Integer(data),ls_Columna) THEN
			MessageBox(	"Atención", "Calle Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1									
		END IF
		IF Duplicado(ls_Columna,data) THEN
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1											
		END IF

								
	CASE "fgmb_base"
		IF NOT validacamara(Integer(data),ls_Columna) THEN
//			MessageBox(	"Atención", "Número de Calle No Existe para la Cámara Seleccionada.~r~nIngrese otro código", &
//											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1									
		END IF	

      IF NOT validaposicioncamara(Integer(data),ls_Columna) THEN
			MessageBox(	"Atención", "Base Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1		
			
		END IF
		
		IF Duplicado(ls_Columna,data) THEN
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1	
			
		END IF

	CASE "fgmb_posici"
		IF NOT validacamara(Integer(data),ls_Columna) THEN
//			MessageBox(	"Atención", "Número de Calle No Existe para la Cámara Seleccionada.~r~nIngrese otro código", &
//											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1	
			
		END IF	

      IF NOT validaposicioncamara(Integer(data),ls_Columna) THEN
			MessageBox(	"Atención", "Altura Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
											Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1		
			
		END IF
		IF Duplicado(ls_Columna,data) THEN
			This.SetItem(il_fila, ls_Columna, li_null)
			RETURN 1											
		END IF
		
END CHOOSE

end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;IF CurrentRow > 0 AND il_fila > 0 THEN
	ias_campo[1] = String(dw_1.Object.clie_codigo[il_fila])
	ias_campo[2] = String(dw_1.Object.plde_codigo[il_fila])

END IF
end event

event dw_1::dberror;call super::dberror;String	err_type,err_msg
window	win

CHOOSE CASE buffer
	CASE delete!
		err_type = "Borrando"
	CASE primary!
		dwitemstatus stat
		stat = This.getitemstatus(row,0,buffer)
		
		IF stat = new! OR stat = newmodified! THEN
			err_type = "Agregando"
		ELSE
			err_type = "Actualizando"
		END IF
END CHOOSE

err_msg = "Error en " + err_type + " registro " + String(row)
err_msg = err_msg + "~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
err_msg = err_msg + "~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

win = Parent

f_errorBaseDatos(sqlca, err_msg)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type gb_4 from groupbox within w_mant_mues_binscamara
integer x = 1294
integer y = 188
integer width = 1595
integer height = 180
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición De Bins"
end type

type st_3 from statictext within w_mant_mues_binscamara
integer x = 224
integer y = 96
integer width = 238
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_binscamara
integer x = 1705
integer y = 92
integer width = 402
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

type st_1 from statictext within w_mant_mues_binscamara
integer x = 224
integer y = 260
integer width = 238
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
string text = "Tarja"
boolean focusrectangle = false
end type

type cbx_todos from checkbox within w_mant_mues_binscamara
integer x = 878
integer y = 252
integer width = 274
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	sle_tarjas.Enabled =	False
	gb_4.visible		 	=	True
	rb_com.visible 		=	True
	rb_todas.Checked	=	True
	rb_gra.visible 		=	True
	rb_todas.visible 	=	True
	il_tarja				=	-1
	ii_estado 			= 	-1
	rb_com.SetFocus()
ELSE
	sle_tarjas.Enabled = 	True
	rb_com.visible 		=	False
	rb_gra.visible 		=	False
	rb_todas.visible 	=	False
	gb_4.visible 		=	False
	il_tarja				=	0
	ii_estado				= 	-1	
	sle_tarjas.SetFocus()
END IF
end event

type rb_com from radiobutton within w_mant_mues_binscamara
boolean visible = false
integer x = 2382
integer y = 252
integer width = 402
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Comercial"
end type

event clicked;IF This.Checked THEN
	ii_estado 				= 	2
END IF
end event

type rb_gra from radiobutton within w_mant_mues_binscamara
boolean visible = false
integer x = 1957
integer y = 252
integer width = 366
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Granel"
end type

event clicked;IF This.Checked THEN
	ii_estado 				= 	1
END IF
end event

type sle_tarjas from editmask within w_mant_mues_binscamara
integer x = 485
integer y = 248
integer width = 352
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "00000000"
end type

event modified;IF IsNumber(this.text) and long(This.text) > 0 THEN
	il_tarja	= 	Long(This.Text)
	ii_estado	= 	-1
ELSE
	This.Text = ''
	This.SetFocus()
END IF
end event

type rb_todas from radiobutton within w_mant_mues_binscamara
boolean visible = false
integer x = 1522
integer y = 252
integer width = 261
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	ii_estado 				= 	-1
END IF
end event

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_binscamara
event destroy ( )
integer x = 1970
integer y = 76
integer height = 100
integer taborder = 70
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -1
		
	Case Else
		idwc_camara.SetTransObject(sqlca)
		idwc_camara.Retrieve(This.Codigo)
		
End Choose

end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_binscamara
event destroy ( )
integer x = 466
integer y = 80
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

