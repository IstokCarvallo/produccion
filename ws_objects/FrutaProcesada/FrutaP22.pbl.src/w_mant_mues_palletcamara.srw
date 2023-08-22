$PBExportHeader$w_mant_mues_palletcamara.srw
forward
global type w_mant_mues_palletcamara from w_mant_directo
end type
type st_3 from statictext within w_mant_mues_palletcamara
end type
type st_4 from statictext within w_mant_mues_palletcamara
end type
type st_1 from statictext within w_mant_mues_palletcamara
end type
type em_numero from editmask within w_mant_mues_palletcamara
end type
type cbx_1 from checkbox within w_mant_mues_palletcamara
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_palletcamara
end type
type uo_selplantas from uo_seleccion_plantas within w_mant_mues_palletcamara
end type
end forward

global type w_mant_mues_palletcamara from w_mant_directo
integer width = 3273
integer height = 1892
string title = "MANTENCION ESTIBA EN CAMARAS"
st_3 st_3
st_4 st_4
st_1 st_1
em_numero em_numero
cbx_1 cbx_1
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_mant_mues_palletcamara w_mant_mues_palletcamara

type variables
DataWindowChild		idwc_camara

Integer					ii_orden, ii_tipocamara
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
public function boolean validacamara (integer valor, string columna)
public function boolean validaposicioncamara (integer valor, string columna)
public function boolean existepallet (long al_pallet, boolean ab_mensaje)
public function boolean tipo_camara (integer ai_valor)
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_Fila, ll_Pallet
Integer	li_Camara, li_Calle, li_Base, li_Posici

li_Camara	=	dw_1.Object.cama_codigo[il_fila]
li_Calle		= 	dw_1.Object.paen_calle[il_fila]
li_Base		= 	dw_1.Object.paen_base[il_fila]
li_Posici	= 	dw_1.Object.paen_posici[il_fila]

CHOOSE CASE columna
	CASE "cama_codigo"
		li_Camara	= Integer(valor)

	CASE "paen_calle"
		li_Calle		= Integer(valor)

	CASE "paen_base"
		li_Base		= Integer(valor)

	CASE "paen_posici"
		li_Posici	= Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("cama_codigo = " + String(li_Camara) + &
							" and " + "paen_calle = " + String(li_Calle) + &
							" and " + "paen_base = " + String(li_Base) + &
							" and " + "paen_posici = " + String(li_posici),1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Posición en Cámara Ya Fue Ingresada Anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean validacamara (integer valor, string columna);Long		ll_Fila, ll_Pallet
Integer	li_Camara, li_Calle, li_Base, li_Posici, li_cama, &
 			li_Calle2, li_Base2, li_Posici2


li_Camara	=	dw_1.Object.cama_codigo[il_fila]
li_Calle		= 	dw_1.Object.paen_calle[il_fila]
li_Base		= 	dw_1.Object.paen_base[il_fila]
li_Posici	= 	dw_1.Object.paen_posici[il_fila]


CHOOSE CASE columna
	CASE "cama_codigo"
		li_Camara	= Integer(valor)

	CASE "paen_calle"
		li_Calle		= Integer(valor)

	CASE "paen_base"
		li_Base		= Integer(valor)

	CASE "paen_posici"
		li_Posici	= Integer(valor)

END CHOOSE

SELECT cama_cancal, cama_canbas, cama_canpos, cama_tipoca
INTO :li_calle2, :li_base2, :li_posici2, :ii_tipocamara
FROM dbo.camarasbode
WHERE cama_codigo = :li_camara
AND   plde_codigo = :uo_SelPlantas.Codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CamarasBode")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox(	"Atención", "Código de Cámara no ha sido creado.~r~nIngrese otro código", &
									Exclamation!, Ok!)
	RETURN False
ELSE

	IF (li_Calle > li_Calle2) OR (li_Base > li_Base2) OR (li_Posici > li_Posici2) THEN
		RETURN False
	ELSE
		RETURN True
	END IF
END IF
end function

public function boolean validaposicioncamara (integer valor, string columna);Long		ll_Fila, ll_Pallet
Integer	li_Camara, li_Calle, li_Base, li_Posici, li_cama, li_Calle2, li_Base2, li_Posici2


li_Camara	=	dw_1.Object.cama_codigo[il_fila]
li_Calle		= 	dw_1.Object.paen_calle[il_fila]
li_Base		= 	dw_1.Object.paen_base[il_fila]
li_Posici	= 	dw_1.Object.paen_posici[il_fila]

CHOOSE CASE columna
	CASE "cama_codigo"
		li_Camara	= Integer(valor)

	CASE "paen_calle"
		li_Calle		= Integer(valor)

	CASE "paen_base"
		li_Base		= Integer(valor)

	CASE "paen_posici"
		li_Posici	= Integer(valor)

END CHOOSE

SELECT paen_numero, paen_calle, paen_base, paen_posici
INTO :ll_Pallet, :li_calle2, :li_base2, :li_posici2
FROM dbo.palletencab
WHERE cama_codigo = :li_Camara
AND   plde_codigo = :uo_SelPlantas.Codigo
AND   paen_calle  = :li_Calle
AND   paen_base   = :li_Base
AND   paen_posici = :li_Posici
AND 	paen_estado	=	1
AND   paen_calle+paen_base+paen_posici>0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN False
ELSE
   UPDATE dbo.palletencab SET
	paen_calle  = 0,
	paen_base   = 0,
	paen_posici = 0
	WHERE cama_codigo = :li_Camara
	AND   plde_codigo = :uo_SelPlantas.Codigo
	AND   paen_calle  = :li_Calle
	AND   paen_base   = :li_Base
	AND   paen_posici = :li_Posici
	AND   paen_numero = :ll_Pallet	
	AND 	paen_estado	=	1;
	
	commit;
END IF


RETURN False
end function

public function boolean existepallet (long al_pallet, boolean ab_mensaje);long ll_pallet,ll_count = 0
boolean lb_Retorno = False


SELECT paen_estado   
INTO :ll_count
FROM dbo.palletencab  
WHERE clie_codigo = :uo_SelCliente.Codigo  AND  
      paen_numero = :al_Pallet AND
     	plde_codigo = :uo_SelPlantas.Codigo AND
		paen_estado = 1;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla palletencab")
	lb_Retorno	=	True
ELSEIF sqlca.SQLCode = 100 THEN
	IF ll_count = 0 THEN
		MessageBox("Atención", "Número de Pallet " + String(al_Pallet, '00000000') + &
					", no ha sido registrado o no esta en Existencia.~r~rIngrese o seleccione otro Número.")
		lb_Retorno = True			
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean tipo_camara (integer ai_valor);Long		ll_Fila, ll_Pallet
Integer	li_Camara

li_Camara	=	ai_valor

SELECT cama_tipoca
INTO :ii_tipocamara
FROM dbo.camarasbode
WHERE cama_codigo = :li_camara
AND   plde_codigo = :uo_SelPlantas.Codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CamarasBode")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox(	"Atención", "Código de Cámara no ha sido creado.~r~nIngrese otro código", &
									Exclamation!, Ok!)
	RETURN False
ELSE
	RETURN True
END IF
end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo,Long(istr_mant.argumento[4]))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
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

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	istr_mant.dw				= dw_1
	
	buscar			= "Código Material:Nmate_codigo,Nombre Material:Smateriales_mate_nombre,Tipo Consumo:Nmapr_tipcon"
	ordenar			= "Código Material:mate_codigo,Nombre Material:materiales_mate_nombre,Tipo Consumo:mapr_tipcon"
End If
end event

on w_mant_mues_palletcamara.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.st_1=create st_1
this.em_numero=create em_numero
this.cbx_1=create cbx_1
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.em_numero
this.Control[iCurrent+5]=this.cbx_1
this.Control[iCurrent+6]=this.uo_selcliente
this.Control[iCurrent+7]=this.uo_selplantas
end on

on w_mant_mues_palletcamara.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.cbx_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila,"clie_codigo", uo_SelCliente.Codigo)
dw_1.SetItem(il_fila,"plde_codigo", uo_SelPlantas.Codigo)

//dw_1.SetColumn("enva_tipoen")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String	ls_argumento
str_info	lstr_info

lstr_info.titulo	= "PALLET EN CAMARAS"
lstr_info.copias	= 1
OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_palletcamara"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,Long(istr_mant.argumento[4]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long ll_Fila

For ll_Fila = 1 To dw_1.RowCount()
	dw_1.Object.paen_fecpro[ll_Fila] = DateTime(Today(), Now())
	dw_1.Object.paen_fecter[ll_Fila] = DateTime(Today(), Now())
Next
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_palletcamara
integer x = 82
integer y = 64
integer width = 2656
integer height = 416
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_palletcamara
integer x = 2802
integer y = 368
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelPlantas.Bloquear(False)

em_numero.Text		= ''
pb_lectura.Enabled 	= False

il_fila					= 0

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_palletcamara
integer x = 2802
integer y = 88
integer taborder = 50
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelPlantas.Bloquear(True)

dw_1.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(sqlca)
idwc_camara.Retrieve(uo_SelPlantas.Codigo)
dw_1.InsertRow(0)
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_palletcamara
boolean visible = false
integer x = 2802
integer y = 752
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_palletcamara
boolean visible = false
integer x = 2802
integer y = 572
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_palletcamara
integer x = 2802
integer y = 1496
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_palletcamara
integer x = 2802
integer y = 1112
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_palletcamara
integer x = 2802
integer y = 932
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_palletcamara
integer x = 82
integer y = 504
integer width = 2656
integer height = 1196
integer taborder = 70
string dataobject = "dw_mues_palletcamara"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null
String	ls_Columna

SetNull(li_null)

ls_Columna	=	dwo.Name

CHOOSE Case ls_Columna
	Case "cama_codigo"	
		dw_1.Object.paen_calle[row] = 0
		dw_1.Object.paen_base[row] = 0
		dw_1.Object.paen_posici[row] = 0
				
		Tipo_camara(Integer(data))
		
		If ValidaPosicionCamara(Integer(data),ls_Columna) Then
			MessageBox("Atención", "Posicion Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		Else						
			If ii_tipocamara = 1 Then
				This.SetItem(il_fila, 'frio_codigo', '2')
			Else
				This.SetItem(il_fila, 'frio_codigo', '6')
			End If	
		End If
		
		If Duplicado(ls_Columna,data) Then
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1											
		End If
		
	Case "paen_calle"
	  If Not IsNull(data) Then	
		If Not ValidaCamara(Integer(data),ls_Columna) Then
			MessageBox(	"Atención", "Número de Calle No Existe para la Cámara Seleccionada.~r~nIngrese otro código", &
									Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		End If	

      If validaposicioncamara(Integer(data),ls_Columna) Then
			MessageBox(	"Atención", "Calle Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
									Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		End If
		If Duplicado(ls_Columna,data) Then
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1											
		End If

     End If	
	  
	Case "paen_base"
 	  If Not IsNull(data) Then		
		If Not ValidaCamara(Integer(data),ls_Columna) Then
			MessageBox(	"Atención", "Número de Base No Existe para la Cámara Seleccionada.~r~nIngrese otro código", &
									Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		End If	

      If validaposicioncamara(Integer(data),ls_Columna) Then
			MessageBox(	"Atención", "Base Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
									Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		End If
		If Duplicado(ls_Columna,data) Then
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1											
		End If
	  End If
	  
	Case "paen_posici"
	  If Not IsNull(data) Then	
		If Not ValidaCamara(Integer(data),ls_Columna) Then
			MessageBox(	"Atención", "Número de Posicion No Existe para la Cámara Seleccionada.~r~nIngrese otro código", &
									Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		End If	

      If validaposicioncamara(Integer(data),ls_Columna) Then
			MessageBox(	"Atención", "Altura Asignada para la Cámara Seleccionada está Ocupada.~r~nIngrese otro código", &
									Exclamation!, Ok!)
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1									
		End If
		If Duplicado(ls_Columna,data) Then
			This.SetItem(il_fila, ls_Columna, li_null)
			Return 1											
		End If
	  End If
End CHOOSE

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

type st_3 from statictext within w_mant_mues_palletcamara
integer x = 411
integer y = 136
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_palletcamara
integer x = 411
integer y = 260
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

type st_1 from statictext within w_mant_mues_palletcamara
integer x = 411
integer y = 376
integer width = 265
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
string text = "Nº Pallet"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_mues_palletcamara
integer x = 754
integer y = 344
integer width = 402
integer height = 96
integer taborder = 50
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
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text
IF ExistePallet(Long(istr_Mant.Argumento[4]),False) THEN
	em_numero.Text = ''
	pb_lectura.Enabled = False
ELSE
	pb_lectura.Enabled = True
END IF	

end event

type cbx_1 from checkbox within w_mant_mues_palletcamara
integer x = 1193
integer y = 360
integer width = 325
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean lefttext = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	em_numero.Enabled = False
	em_numero.Text = ''
	istr_mant.argumento[4] = '-1'
	pb_lectura.Enabled = True
ELSE
	em_numero.Enabled = True
	em_numero.SetFocus()
	IF em_numero.text <> '' THEN 
		pb_lectura.Enabled = True
	ELSE
		pb_lectura.Enabled = False
	END IF	
END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_palletcamara
event destroy ( )
integer x = 754
integer y = 124
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_mant_mues_palletcamara
event destroy ( )
integer x = 754
integer y = 248
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

idwc_camara.SetTransObject(sqlca)

Choose Case This.Codigo
	Case -1, -9
		idwc_camara.Retrieve(-1)
		
	Case Else 
		idwc_camara.Retrieve(This.Codigo)
End Choose

end event

