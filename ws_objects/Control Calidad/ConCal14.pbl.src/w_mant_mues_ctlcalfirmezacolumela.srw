$PBExportHeader$w_mant_mues_ctlcalfirmezacolumela.srw
$PBExportComments$Mantenedor de Materia Seca por Especie
forward
global type w_mant_mues_ctlcalfirmezacolumela from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_ctlcalfirmezacolumela
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcalfirmezacolumela
end type
end forward

global type w_mant_mues_ctlcalfirmezacolumela from w_mant_directo
string tag = "Maestro Firmeza Columela"
integer width = 2043
integer height = 1916
string title = "Maestro Firmeza Columela"
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_ctlcalfirmezacolumela w_mant_mues_ctlcalfirmezacolumela

type variables

end variables

forward prototypes
public function boolean duplicado (string valor)
end prototypes

public function boolean duplicado (string valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	Integer(valor)

ll_fila	= dw_1.Find("fico_codigo = " + String(li_codigo),1 , dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelEspecie.Codigo)

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
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

IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(False,False)
	uo_SelEspecie.dw_Seleccion.SetFocus()
	
	buscar			= "Codigo:Nfico_codigo,Descripcion:Sfico_nombre"
	ordenar			= "Codigo:fico_codigo,Descripcion:fico_nombre"
End If
end event

on w_mant_mues_ctlcalfirmezacolumela.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selespecie
end on

on w_mant_mues_ctlcalfirmezacolumela.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selespecie)
end on

event ue_imprimir;SetPointer(HourGlass!)

Integer	li_zona
Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE TEMPERATURA Y NIVEL DE GASES."
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalfirmezacolumela"
vinf.dw_1.SetTransObject(sqlca)


fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;Long		ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]

For ll_Fila	= 1 To dw_1.RowCount()
	If dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! Then
		dw_1.DeleteRow(ll_fila)
	Else
		dw_1.Object.espe_codigo[ll_Fila] = uo_SelEspecie.Codigo
	End If
Next

For ll_Fila	= 1 To dw_1.RowCount()
	If Isnull(dw_1.Object.fico_codigo[ll_Fila]) OR dw_1.Object.fico_codigo[ll_Fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo "
		ls_colu[li_cont]	= "fico_codigo"
	End If
	
	If Isnull(dw_1.Object.fico_nombre[ll_Fila]) OR trim(dw_1.Object.fico_nombre[ll_Fila]) = "" Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nDescripción "
		ls_colu[li_cont]	= "fico_nombre"
	End If

Next

	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	else
		 Message.DoubleParm = 0		
	End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetColumn('fico_codigo')
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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalfirmezacolumela
integer x = 82
integer y = 52
integer width = 1499
integer height = 220
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalfirmezacolumela
integer x = 1696
integer y = 404
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;pb_insertar.Enabled	= False
pb_grabar.Enabled	= False
pb_eliminar.Enabled	= False
pb_imprimir.Enabled	= False
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalfirmezacolumela
integer x = 1701
integer y = 136
integer taborder = 30
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalfirmezacolumela
integer x = 1701
integer y = 792
integer taborder = 60
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalfirmezacolumela
integer x = 1701
integer y = 612
integer taborder = 50
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalfirmezacolumela
integer x = 1701
integer y = 1536
integer taborder = 90
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalfirmezacolumela
integer x = 1701
integer y = 1152
integer taborder = 80
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalfirmezacolumela
integer x = 1701
integer y = 972
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalfirmezacolumela
integer x = 82
integer y = 292
integer width = 1499
integer height = 1472
integer taborder = 20
string dataobject = "dw_mues_ctlcalfirmezacolumela"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Long	ll_Null
String	ls_Columna

ls_Columna	=	dwo.Name
SetNull(ll_Null)

Choose Case ls_Columna	
	Case "fico_codigo"
		If Duplicado(data) Then
			This.SetItem(il_fila, ls_Columna, ll_Null)
			Return 1
		End If
	End Choose

end event

event dw_1::sqlpreview;//
end event

type st_1 from statictext within w_mant_mues_ctlcalfirmezacolumela
integer x = 142
integer y = 132
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcalfirmezacolumela
event destroy ( )
integer x = 421
integer y = 112
integer height = 96
integer taborder = 10
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

