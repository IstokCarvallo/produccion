$PBExportHeader$w_mant_mues_bitacoraexis_val.srw
forward
global type w_mant_mues_bitacoraexis_val from w_mant_directo
end type
type dw_2 from uo_dw within w_mant_mues_bitacoraexis_val
end type
end forward

global type w_mant_mues_bitacoraexis_val from w_mant_directo
integer width = 4718
string title = "Validacion Existencia Consolidada"
dw_2 dw_2
end type
global w_mant_mues_bitacoraexis_val w_mant_mues_bitacoraexis_val

type variables
DataWindowChild	idwc_Planta, idwc_Horario

Datastore     		ds_Evento
end variables

forward prototypes
public function boolean duplicado (string codigo)
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public function boolean wf_recupera (date ad_fecha, time at_hora)
public subroutine wf_color (datawindow adw)
end prototypes

public function boolean duplicado (string codigo);Long		ll_fila
String	ls_codigo

	ls_codigo	= String(dw_1.GetItemNumber(il_fila,"siri_codigo"))
	
	ll_fila	= dw_1.Find("siri_codigo = " +  codigo , 1, dw_1.RowCount())
	
	
	IF ll_fila > 0 and ll_fila <> il_fila THEN
		MessageBox("Error","Código de Sistema de Riego, ya fue ingresado anteriormente",Information!, Ok!)
		RETURN True
	ELSE
		RETURN False
	END IF

		
end function

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public function boolean wf_recupera (date ad_fecha, time at_hora);Long	ll_Existe, ll_Planta
String	ls_Usuario
Time	lt_Hora
Date	ld_Fecha	
  
SELECT Count(*)
	Into :ll_Existe
 FROM dbo.BitacoraExis
WHERE biex_fecham = :ad_Fecha
  AND eval_horalr = :at_hora;
  
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalrecepcionfrutasenca  ")
 	Return False
ElseIf sqlca.sqlcode	=	0 Then
	If ll_Existe > 0 Then Return True
Else
	Return False
End If
end function

public subroutine wf_color (datawindow adw);Long	ll_Verde, ll_Amarillo, ll_Naranjo, ll_Rojo, ll_Fila, ll
Time	lt_Limite, lt_Tolerancia, lt_Valor

ll_Verde		=	RGB(0,255,0)
ll_Amarillo	=	RGB(255,255,0)
ll_Naranjo	=	RGB(255,128,0)
ll_Rojo		=	RGB(255,0,0)

For ll_Fila = 1 To adw.RowCount()
	lt_Limite			= adw.Object.eval_horalr[ll_Fila]
	lt_Tolerancia	= adw.Object.eval_tolera[ll_Fila]
	lt_Valor			= Time(adw.Object.biex_fechav[ll_Fila])
	
	If IsNull(adw.Object.biex_valida[ll_Fila]) Or adw.Object.biex_valida[ll_Fila] = 0 Then
		adw.Object.Color[ll_Fila] = ll_Rojo
	Else
		ll =  SecondsAfter(lt_Limite, lt_Valor)
		If SecondsAfter(lt_Limite, lt_Valor) <= 0 Then
			adw.Object.Color[ll_Fila] = ll_Verde
		ElseIf SecondsAfter(lt_Limite, lt_Valor) > 0 And SecondsAfter(lt_Tolerancia, lt_Valor) <= 0 Then
			adw.Object.Color[ll_Fila] = ll_Amarillo
		Else
			adw.Object.Color[ll_Fila] = ll_Naranjo
		End If
	End If
Next

Return
end subroutine

on w_mant_mues_bitacoraexis_val.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_mant_mues_bitacoraexis_val.destroy
call super::destroy
destroy(this.dw_2)
end on

event open;call super::open;buscar			=	"Tipo Evento:Nevex_tipova,Evento:Sevex_nombre"
ordenar			=	"Tipo Evento:evex_tipova,Evento:evex_nombre"
is_ultimacol		=	'biex_observ'


dw_2.GetChild("plde_codigo", idwc_Planta)
idwc_Planta.SetTransObject(Sqlca)
idwc_Planta.Retrieve()

dw_2.GetChild("eval_horalr", idwc_Horario)
idwc_Horario.SetTransObject(Sqlca)
idwc_Horario.Retrieve()

dw_2.InsertRow(0)

dw_2.Object.usua_codigo[1]	=	gstr_us.Nombre
dw_2.Object.biex_fecham[1]	=	Today()

ds_Evento	=	Create Datastore


end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, Respuesta

Do
	ll_fila	= dw_1.Retrieve(dw_2.Object.biex_fecham[1], dw_2.Object.eval_horalr[1])
	
	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila > 0 Then
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		pb_imprimir.Enabled	= True
		
		For ll_Fila = 1 To dw_1.RowCount()
			If IsNull(dw_1.Object.biex_valval[ll_Fila]) Then
				dw_1.SetItemStatus(ll_Fila, 0, Primary!, DataModified!)
			End If
		Next
		
		wf_Color(dw_1)
		
		dw_2.Object.eval_fecha[1]  =	dw_1.Object.biex_fecval[1]
		dw_2.Object.eval_ticket[1]	=	dw_1.Object.biex_valval[1]
		dw_2.Object.eval_observ[1]	=	dw_1.Object.biex_obsval[1]
		
	End If
	
Loop While Respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO VALIDACION EXISTENCIA CONSOLIDADA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_bitacoraexis_val"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(dw_2.Object.biex_fecham[1], dw_2.Object.eval_horalr[1])

wf_Color(vinf.dw_1)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 85')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long	ll_Fila

For ll_Fila = 1 To dw_1.RowCount()
	If	dw_1.GetItemStatus(ll_Fila, 0, Primary!) = DataModified! Then
		dw_1.Object.biex_usaval[ll_Fila]	= dw_2.Object.usua_codigo[1]
		dw_1.Object.biex_fecval[ll_Fila]	= dw_2.Object.eval_fecha[1] 
		dw_1.Object.biex_valval[ll_Fila]	= dw_2.Object.eval_ticket[1]
		dw_1.Object.biex_obsval[ll_Fila]	= dw_2.Object.eval_observ[1]
		dw_1.Object.biex_comval[ll_Fila]	= gstr_us.Computador
	End If	
Next
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_bitacoraexis_val
boolean visible = false
integer x = 2917
integer y = 652
integer width = 174
integer height = 120
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_bitacoraexis_val
string tag = ""
integer x = 4338
integer y = 412
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_bitacoraexis_val
string tag = "Selección de Parámetros"
integer x = 4329
integer y = 176
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_bitacoraexis_val
string tag = ""
boolean visible = false
integer x = 4329
integer y = 768
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_bitacoraexis_val
string tag = ""
boolean visible = false
integer x = 4329
integer y = 588
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_bitacoraexis_val
string tag = ""
integer x = 4329
integer y = 1444
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_bitacoraexis_val
string tag = ""
integer x = 4329
integer y = 1128
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_bitacoraexis_val
string tag = ""
integer x = 4329
integer y = 948
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_bitacoraexis_val
integer x = 27
integer y = 1020
integer width = 4114
integer height = 1152
string dataobject = "dw_mant_mues_bitacoraexis_val"
end type

type dw_2 from uo_dw within w_mant_mues_bitacoraexis_val
integer x = 718
integer y = 20
integer width = 2688
integer height = 868
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_bitacoraexis_val"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;String	ls_Columna 

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "biex_fecham"
		If wf_Recupera(Date(Data), This.Object.eval_horalr[1]) Then
			Parent.PostEvent("ue_recuperadatos")
		End If
		
	Case "eval_horalr"
		If wf_Recupera(This.Object.biex_fecham[1], Time(Data)) Then
			Parent.PostEvent("ue_recuperadatos")
		End If
	
	Case "eval_ticket"
		If Data = "1" Then 
			This.Object.eval_fecha[Row]	=	Today()
		End If		
		
End Choose 


end event

