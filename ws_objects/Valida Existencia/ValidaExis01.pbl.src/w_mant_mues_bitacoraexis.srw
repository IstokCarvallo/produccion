$PBExportHeader$w_mant_mues_bitacoraexis.srw
forward
global type w_mant_mues_bitacoraexis from w_mant_directo
end type
type dw_2 from uo_dw within w_mant_mues_bitacoraexis
end type
end forward

global type w_mant_mues_bitacoraexis from w_mant_directo
integer width = 4210
string title = "Validacion Existencia Planta"
dw_2 dw_2
end type
global w_mant_mues_bitacoraexis w_mant_mues_bitacoraexis

type variables
DataWindowChild	idwc_Planta, idwc_Horario

Datastore     		ds_Evento
end variables

forward prototypes
public function boolean duplicado (string codigo)
public function long wf_recupera_eventos ()
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public function boolean wf_recupera (string as_usuario, long al_planta, date ad_fecha, time at_hora)
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

public function long wf_recupera_eventos ();Long    	ll_Fila, ll_Filad, ll_New, ll_null
String  	ls_Nombre
Integer	li_Tipova, li_Codigo

SetNull(ll_Null)

dw_1.Retrieve(dw_2.Object.usua_codigo[1], dw_2.Object.plde_codigo[1], &
						dw_2.Object.biex_fecham[1], dw_2.Object.eval_horalr[1])

ds_Evento.Dataobject =	'dw_mues_eventoexis'
ds_Evento.SetTransObject(SQLca)

ll_fila	=	ds_Evento.Retrieve()

For ll_fila = 1 To ds_Evento.RowCount()
	 li_Tipova = ds_Evento.Object.evex_tipova[ll_fila]
	 li_Codigo = ds_Evento.Object.evex_codigo[ll_fila]
	 ls_Nombre = ds_Evento.Object.evex_nombre[ll_fila]
	 
	 ll_Filad	= dw_1.Find("evex_tipova = " + String(li_Tipova) + " And evex_codigo = " + String(li_Codigo), 1, dw_1.RowCount())
	 	 				
  	 If ll_Filad = 0 Then
		ll_New = dw_1.InsertRow(0)
		dw_1.Object.evex_tipova[ll_New]		= li_Tipova
		dw_1.Object.evex_codigo[ll_New]		= li_Codigo
		dw_1.Object.evex_nombre[ll_New]	= ls_Nombre
	 End If
Next
	 
Return dw_1.RowCount()
end function

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public function boolean wf_recupera (string as_usuario, long al_planta, date ad_fecha, time at_hora);Long	ll_Existe, ll_Planta
String	ls_Usuario
Time	lt_Hora
Date	ld_Fecha	
  
SELECT Count(usua_codigo)
	Into :ll_Existe
 FROM dbo.BitacoraExis
WHERE usua_codigo = :as_Usuario
  AND plde_codigo = :al_planta 
  AND biex_fecham = :ad_Fecha
  AND eval_horalr = :at_hora;
  
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Bitacora Existencia  ")
 	Return False
ElseIf sqlca.sqlcode	=	0 Then
	If ll_Existe > 0 Then Return True
Else
	Return False
End If
end function

on w_mant_mues_bitacoraexis.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_mant_mues_bitacoraexis.destroy
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
dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.biex_fecham[1]	=	Today()

ds_Evento	=	Create Datastore


end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, Respuesta

Do
	ll_fila	= dw_1.Retrieve(dw_2.Object.usua_codigo[1], dw_2.Object.plde_codigo[1], &
						dw_2.Object.biex_fecham[1], dw_2.Object.eval_horalr[1])
	
	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		pb_imprimir.Enabled	= True
		il_fila	=	wf_Recupera_Eventos()
	End If
	
Loop While Respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO VALIDACION EXISTENCIA PLANTA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_bitacoraexis"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(dw_2.Object.usua_codigo[1], dw_2.Object.plde_codigo[1], &
						dw_2.Object.biex_fecham[1], dw_2.Object.eval_horalr[1])

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else	
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long	ll_Fila

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.GetItemStatus(ll_Fila, 0, Primary!) = New! Or &
		dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! Then
		
		dw_1.Object.usua_codigo[ll_Fila]	= dw_2.Object.usua_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila]	= dw_2.Object.plde_codigo[1]
		dw_1.Object.biex_fecham[ll_Fila]	= dw_2.Object.biex_fecham[1]
		dw_1.Object.eval_horalr[ll_Fila]	= dw_2.Object.eval_horalr[1]
		dw_1.Object.biex_comput[ll_Fila]	= gstr_us.Computador
		
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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_bitacoraexis
boolean visible = false
integer x = 329
integer y = 1640
integer width = 174
integer height = 120
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_bitacoraexis
string tag = ""
integer x = 3822
integer y = 372
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_bitacoraexis
string tag = "Selección de Parámetros"
integer x = 3813
integer y = 136
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_bitacoraexis
string tag = ""
boolean visible = false
integer x = 3813
integer y = 728
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_bitacoraexis
string tag = ""
boolean visible = false
integer x = 3813
integer y = 548
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_bitacoraexis
string tag = ""
integer x = 3813
integer y = 1404
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_bitacoraexis
string tag = ""
integer x = 3813
integer y = 1088
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_bitacoraexis
string tag = ""
integer x = 3813
integer y = 908
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_bitacoraexis
integer x = 46
integer y = 624
integer width = 3589
integer height = 1400
string dataobject = "dw_mant_mues_bitacoraexis"
end type

event dw_1::itemchanged;
Choose Case dwo.Name
	Case "biex_valida"
		If Data = "1" Then 
			This.Object.biex_fechav[Row]	=	Today()
			If IsNull(This.Object.biex_valore[Row]) Then This.Object.biex_valore[Row] = 0
		End If		
End Choose 
end event

type dw_2 from uo_dw within w_mant_mues_bitacoraexis
integer x = 1239
integer y = 56
integer width = 1390
integer height = 492
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_bitacoraexis"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;String	ls_Columna 

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "biex_fecham"
		If wf_Recupera(This.Object.usua_codigo[1],dw_2.Object.plde_codigo[1], Date(Data), This.Object.eval_horalr[1]) Then
			Parent.PostEvent("ue_recuperadatos")
		End If
		
	Case "eval_horalr"
		If wf_Recupera(This.Object.usua_codigo[1],dw_2.Object.plde_codigo[1], This.Object.biex_fecham[1], Time(Data)) Then
			Parent.PostEvent("ue_recuperadatos")
		End If		
		
End Choose 
end event

