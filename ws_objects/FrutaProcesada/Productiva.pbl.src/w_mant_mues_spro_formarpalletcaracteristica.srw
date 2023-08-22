$PBExportHeader$w_mant_mues_spro_formarpalletcaracteristica.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_spro_formarpalletcaracteristica from w_mant_directo
end type
end forward

global type w_mant_mues_spro_formarpalletcaracteristica from w_mant_directo
integer width = 3529
string title = "MAESTRO FORMAR PALLET CARACTERISTICAS"
end type
global w_mant_mues_spro_formarpalletcaracteristica w_mant_mues_spro_formarpalletcaracteristica

type variables
Integer				ii_tipo, ii_orden

uo_grabatablas		iuo_grabatablas
uo_Clientesprod	iuo_Cliente
uo_EmbalajesProd	iuo_Embalaje
uo_Plantadesp		iuo_Planta
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
public subroutine wf_replicacion ()
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_cliente, li_planta
String	ls_embalaje

li_planta	=	dw_1.Object.plde_codigo[il_fila]
li_cliente	=	dw_1.Object.clie_codigo[il_fila]
ls_embalaje	=	dw_1.Object.emba_codigo[il_fila]

CHOOSE CASE columna
	CASE "clie_codigo"
		li_cliente	=	Integer(valor)
	
	CASE "plde_codigo"
		li_planta	=	Integer(valor)
	
	CASE "emba_codigo"
		ls_embalaje	= 	Valor

END CHOOSE

ll_fila	= dw_1.Find("clie_codigo = " + String(li_cliente) + &
							"and plde_codigo = " + String(li_planta) + &
							"and emba_codigo = '" + String(ls_embalaje)+"'", + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public subroutine wf_replicacion ();Integer	ll_fila
ll_fila = dw_1.GetNextModified(ll_fila, Primary!)
IF ll_fila > 0 THEN
	IF iuo_grabatablas.existereplicatablas(gi_CodExport) AND gstr_apl.CodigoSistema = 23 THEN
		iuo_grabatablas.replicatabla_formarpalletcaracteristica(dw_1)
	END IF	
END IF	

end subroutine

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
//		dw_1.SetRow(1)
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

event open;call super::open;
istr_mant.dw				= dw_1

buscar			= "Planta :Splde_codigo,Embalaje:Semba_codigo"
ordenar			= "Planta :plde_codigo,Embalaje:emba_descri"

iuo_grabatablas	=	Create uo_grabatablas
iuo_Cliente			=	Create uo_Clientesprod
iuo_Embalaje		=	Create uo_EmbalajesProd
iuo_Planta			=	Create uo_Plantadesp

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
		istr_mant.Solo_Consulta = True
	END IF	
END IF	

end event

on w_mant_mues_spro_formarpalletcaracteristica.create
call super::create
end on

on w_mant_mues_spro_formarpalletcaracteristica.destroy
call super::destroy
end on

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

   IF Isnull(dw_1.Object.clie_codigo[il_fila]) OR dw_1.Object.clie_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Cliente"
		ls_colu[li_cont]	= "clie_codigo"
	END IF
	
	IF Isnull(dw_1.Object.plde_codigo[il_fila]) OR dw_1.Object.plde_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Planta"
		ls_colu[li_cont]	= "plde_codigo"
	END IF
	
	IF Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
		ls_colu[li_cont]	= "emba_codigo"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "FORMAR PALLET CARACTERISTICAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_formarpalletcaracteristicas"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve()

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

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF dw_1.RowCount() > 0 THEN
	
	IF Isnull(dw_1.Object.clie_codigo[il_fila]) OR dw_1.Object.clie_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Cliente"
		ls_colu[li_cont]	= "clie_codigo"
	END IF
	
	IF Isnull(dw_1.Object.plde_codigo[il_fila]) OR dw_1.Object.plde_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "Código nPlanta"
		ls_colu[li_cont]	= "plde_codigo"
	END IF
	
	IF Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
		ls_colu[li_cont]	= "emba_codigo"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF

ELSE
	pb_Grabar.Enabled		=	False
	pb_Eliminar.Enabled	=	False
	pb_Imprimir.Enabled	=	False
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_formarpalletcaracteristica
boolean visible = false
integer x = 197
integer y = 492
integer width = 2674
integer height = 244
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 360
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
	//	IF gi_codplanta <> 2 AND gi_codplanta <> 3 AND gi_codplanta <> 4 THEN
			istr_mant.Solo_Consulta = True
	//	END IF	
	END IF	
END IF	


end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 88
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 744
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 564
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 1488
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 1104
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_formarpalletcaracteristica
integer x = 3104
integer y = 924
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_formarpalletcaracteristica
integer x = 82
integer y = 68
integer width = 2871
integer height = 1292
integer taborder = 70
string dataobject = "dw_mues_spro_formarpalletcaracteristica"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Null, ls_Columna

ls_Columna = dwo.Name
SetNull(ls_Null)

Choose Case ls_Columna
	Case "emba_codigo"	
		If Not iuo_Embalaje.Existe(iuo_Cliente.Codigo, data, True, Sqlca)  Or Duplicado('emba_codigo',data) Then
			This.SetItem(Row, ls_Columna, ls_Null)
			Return 1
		End If
			
	Case "clie_codigo"	
		If Not iuo_Cliente.Existe(Integer(data), True, Sqlca) Or Duplicado('clie_codigo',data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If	
				
	Case "plde_codigo"	
		If Not iuo_Planta.Existe(Integer(data), True, SQLca) Or Duplicado('plde_codigo',data)  Then
			This.SetItem(il_fila,"plde_codigo",Integer(ls_Null))
			Return 1
		End If
		
End Choose

end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;//IF CurrentRow > 0 AND il_fila > 0 THEN
//	ias_campo[1] = String(dw_1.Object.cctc_codigo[il_fila])
//	ias_campo[2] = dw_1.Object.cctc_nombres[il_fila]
//	ias_campo[3] = dw_1.Object.cctc_abrevi[il_fila]
//END IF

Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;CHOOSE CASE dwo.Name

	CASE "copa_nombre"
			pb_grabar.Enabled	=	True

//	CASE "cctc_codigo"
//			TriggerEvent("ue_validaregistro")

END CHOOSE

end event

event dw_1::clicked;call super::clicked;CHOOSE CASE dwo.Name
	CASE "b_embalaje"	
		Str_busqueda	lstr_busq
		lstr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
		
		OpenWithParm(w_busc_embalajesprod, lstr_busq)
		
		lstr_busq	       = Message.PowerObjectParm
		
		IF lstr_busq.argum[2] <> "" THEN
			dw_1.Object.emba_codigo[row] = lstr_busq.argum[2]
		END IF
END CHOOSE
end event

