$PBExportHeader$w_mant_mues_correltarjacont.srw
forward
global type w_mant_mues_correltarjacont from w_mant_directo
end type
end forward

global type w_mant_mues_correltarjacont from w_mant_directo
integer width = 3136
integer height = 2016
string title = "CORRELATIVO DE DOCUMENTOS"
end type
global w_mant_mues_correltarjacont w_mant_mues_correltarjacont

type variables

end variables

forward prototypes
public function boolean validacorrel (string ls_valor, integer tipo)
public subroutine habilitagrabar ()
public function boolean duplicado (string columna, string valor)
end prototypes

public function boolean validacorrel (string ls_valor, integer tipo);Long	ll_inicio, ll_final, ll_fila

ll_inicio 	=	dw_1.Object.cota_inicio[il_fila]
ll_final	=	dw_1.Object.cota_finali[il_fila]

Choose Case tipo
	Case 1
		ll_inicio	=	Long(ls_valor)
		If ll_inicio > ll_final Then
			MessageBox("Atención","Correlativo Inicial no puede ser Mayor que el Final.", Exclamation!, Ok!)
			Return True
		End If
		
		ll_fila	= dw_1.Find( "cota_inicio < " + String(ll_inicio) +" and cota_finali > " + String(ll_inicio), 1, dw_1.RowCount())
		
		If ll_fila > 0 Then
			MessageBox("Atención","Correlativo Inicial Está contenido en otro rango.", Exclamation!, Ok!)
			Return True
		End If
		
	Case 2
		ll_final	=	Long(ls_valor)
		
		If ll_final < ll_inicio Then
			MessageBox("Atención","Correlativo Final no puede ser Menor que el Inicial.", Exclamation!, Ok!)
			Return True
		End If
		
		ll_fila	= dw_1.Find( "cota_inicio < " + String(ll_final) +" and cota_finali > " + String(ll_final), 1, dw_1.RowCount())
		
		If ll_fila > 0 Then
			MessageBox("Atención","Correlativo Final Está contenido en otro rango.", Exclamation!, Ok!)
			Return True
		End If
		
End Choose

Return False
end function

public subroutine habilitagrabar ();Long		ll_filas
Boolean	Habilita	= True
Datetime	ld_Fecha

dw_1.AcceptText()

If Isnull(dw_1.Object.cont_codigo[il_fila]) OR dw_1.Object.cont_codigo[il_fila] = 0 Then
	Habilita = False
ElseIf Isnull(dw_1.Object.cota_inicio[il_fila]) OR dw_1.Object.cota_inicio[il_fila] = 0 Then
	Habilita = False
ElseIf Isnull(dw_1.Object.cota_finali[il_fila]) OR dw_1.Object.cota_finali[il_fila] = 0 Then
	Habilita = False
ElseIf Isnull(dw_1.Object.cota_fechac[il_fila]) OR dw_1.Object.cota_fechac[il_fila] = ld_Fecha Then
	Habilita = False
End If

pb_eliminar.Enabled	= Habilita
pb_grabar.Enabled	= Habilita
end subroutine

public function boolean duplicado (string columna, string valor);String		ls_Tipo, ls_CorrInicio, ls_Centro
Long		ll_fila

ls_Tipo		=	String(dw_1.Object.cont_codigo[il_fila])
ls_CorrInicio	=	String(dw_1.Object.cota_inicio[il_fila])

CHOOSE CASE columna
	CASE "cont_codigo"
		ls_tipo		= valor
	CASE "cota_inicio"
		ls_CorrInicio	=	valor
End CHOOSE

ll_fila	= dw_1.Find( "cont_codigo = " + ls_Tipo+" and cota_inicio = " + ls_CorrInicio, 1, dw_1.RowCount())

If ll_fila > 0 AND ll_fila <> il_fila Then
	MessageBox("Error","Registro ya fue ingresada anteriormente",Information!, Ok!)
	Return True
Else
	Return False
End If


end function

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True
	ELSE
		pb_lectura.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_correltarjacont.create
call super::create
end on

on w_mant_mues_correltarjacont.destroy
call super::destroy
end on

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila = 1, ll_inicio, ll_final
Datetime	ld_fecha
Boolean	lb_Habilita = True

DO WHILE ll_fila <= dw_1.RowCount()
	
	ll_inicio	=	dw_1.Object.cota_inicio[ll_fila]
	ll_final	=	dw_1.Object.cota_finali[ll_fila]
	
	If IsNull(dw_1.Object.cont_codigo[ll_fila]) OR dw_1.Object.cont_codigo[ll_fila] = 0 Then
		lb_Habilita = False
	ElseIf IsNull(dw_1.Object.cota_inicio[ll_fila]) OR dw_1.Object.cota_inicio[ll_fila] = 0 Then
		lb_Habilita = False
	ElseIf IsNull(dw_1.Object.cota_finali[ll_fila]) OR dw_1.Object.cota_finali[ll_fila] = 0 Then
		lb_Habilita = False
	ElseIf IsNull(dw_1.Object.cota_fechac[ll_fila]) OR dw_1.Object.cota_fechac[ll_fila] = ld_fecha Then
		lb_Habilita = False
	End If
	
	If ll_inicio > ll_final OR ll_final < ll_inicio Then lb_Habilita = False
	
	If lb_Habilita = False Then
		MessageBox("Atención","Inconsistencia de Información en Fila Nro. [" + String(ll_fila) + &
			"].", Exclamation!, Ok!)
		Message.DoubleParm = -1
	End If
		
	ll_fila ++
	lb_Habilita = True
			
LOOP
end event

event ue_imprimir;Long		fila
str_info	lstr_info

lstr_info.titulo	= "CORRELATIVO DE TARJAS CONTRATISTA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_correltarjacont"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve()

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
End If
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

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

event ue_nuevo;il_fila = dw_1.InsertRow(0)

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn(1)

end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_correltarjacont
boolean visible = false
integer x = 82
integer y = 20
integer width = 2446
integer height = 196
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_correltarjacont
integer x = 2642
integer y = 324
integer taborder = 60
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_correltarjacont
integer x = 2711
integer y = 72
integer taborder = 20
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_correltarjacont
integer x = 2711
integer y = 684
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_correltarjacont
integer x = 2711
integer y = 504
integer taborder = 70
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_correltarjacont
integer x = 2711
integer y = 1472
integer taborder = 50
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_correltarjacont
integer x = 2720
integer y = 1088
integer taborder = 40
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_correltarjacont
integer x = 2711
integer y = 908
integer taborder = 30
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_correltarjacont
integer x = 82
integer y = 236
integer width = 2446
integer height = 1636
integer taborder = 10
string dataobject = "dw_mant_mues_correltarjacont"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Long		ll_null
Datetime	ld_null

SetNull(ll_null)
SetNull(ld_null)

ls_columna = dwo.Name

Choose Case ls_columna
	Case "cota_inicio"
		If ValidaCorrel(data,1) OR Duplicado(ls_columna, data) Then
			This.SetItem(il_fila, ls_columna, ll_null)
			Return 1
		End If
		
	Case "cota_finali"
		If ValidaCorrel(data,2) Then
			This.SetItem(il_fila, ls_columna, ll_null)
			Return 1
		End If
		
	Case "cota_fechac"
		If Date(Mid(data,1,10))<Today() Then
			MessageBox("Atención","Fecha de Vigencia debe ser posterior a fecha actual")
			This.SetItem(il_fila, ls_columna, ld_null)
			Return 1
		End If
End Choose

Habilitagrabar()

end event

