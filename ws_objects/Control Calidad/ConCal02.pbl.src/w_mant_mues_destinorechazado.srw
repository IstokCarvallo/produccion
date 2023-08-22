$PBExportHeader$w_mant_mues_destinorechazado.srw
$PBExportComments$Mantenedor de Técnicos
forward
global type w_mant_mues_destinorechazado from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_destinorechazado
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_destinorechazado
end type
end forward

global type w_mant_mues_destinorechazado from w_mant_tabla
integer width = 2414
integer height = 1996
string title = "Destino P. Rechazado"
event ue_validaregistro ( )
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_destinorechazado w_mant_mues_destinorechazado

type variables
uo_especie				iuo_especies


end variables

forward prototypes
public function boolean duplicado (string campo, string valor)
end prototypes

public function boolean duplicado (string campo, string valor);Long	ll_fila, ll_Codigo

ll_Codigo		=	dw_1.Object.dere_codigo[il_fila]

Choose Case Campo
	Case "dere_codigo"
		ll_Codigo		=	Long(valor)
	
End Choose 

ll_fila	= dw_1.Find("dere_codigo = " + String(ll_Codigo), 1, dw_1.RowCount())

If ll_fila > 0 And ll_fila <> il_fila Then
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	Return True
Else
	Return False
End If
end function

on w_mant_mues_destinorechazado.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selespecie
end on

on w_mant_mues_destinorechazado.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selespecie)
end on

event open;call super::open;Boolean	lb_Cerrar
Long		li_especie

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	li_Especie = Message.DoubleParm
	
	uo_SelEspecie.Seleccion(False, False)
	
	If li_Especie <> 0 Then
		uo_SelEspecie.Bloquear(True)
		uo_SelEspecie.Codigo	=	li_Especie
		uo_SelEspecie.dw_Seleccion.Object.Codigo[1]=	li_Especie
	End If
	
	buscar	=	"Código:Ndere_codigo,Nombre:Sdere_nombre"
	ordenar	=	"Código:dere_codigo,Nombre:dere_nombre"
End If
end event

event ue_recuperadatos;Long		ll_fila, respuesta
Integer  li_grupo,li_UsAdmi
String   ls_Usuario

ls_Usuario		=	Upper(Gstr_Us.Nombre)
li_UsAdmi		= Integer(Gstr_Us.Administrador)

DO	
	ll_fila			=	dw_1.Retrieve(uo_SelEspecie.Codigo)
		
	IF ll_fila 		=	-1 THEN
		respuesta 	=	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila >= 0 THEN
		
			li_Grupo = BuscaGrupo(ls_Usuario)					
			IF  li_UsAdmi	=	1 OR (li_Grupo = 1) THEN 			
				dw_1.SetFocus()
				il_fila					= 1
				pb_imprimir.Enabled	= True
				pb_insertar.Enabled	= True
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
			ELSE
				pb_insertar.SetFocus()
				pb_insertar.Enabled	= False	
				pb_imprimir.Enabled	= True
			END IF 	
		END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DESTINO RECHAZADO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_destinorechazado"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
END IF
end event

event ue_nuevo;IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

il_fila = dw_1.InsertRow(0)

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn("dere_codigo")


istr_mant.borra	= False
istr_mant.agrega	= True

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
end event

event ue_antesguardar;Long	ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]

If dw_1.RowCount() > 0 Then
   For ll_fila = 1 To dw_1.RowCount()
		 If dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! Then
			 dw_1.DeleteRow(ll_fila)
		 ELSE
			If dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModIfied! Then
				dw_1.setitem(ll_fila,"espe_codigo", uo_SelEspecie.Codigo)
						
				If IsNull(dw_1.Object.dere_codigo[ll_fila]) Or dw_1.Object.dere_codigo[ll_fila] = 0 Then
					li_cont ++
					ls_mensaje 		= ls_mensaje + "~nCódigo"
					ls_colu[li_cont]	= "dere_codigo"
				End If
				
				If IsNull(dw_1.Object.dere_nombre[ll_fila]) Or trim(dw_1.Object.dere_nombre[ll_fila]) = '' Then
					li_cont ++
					ls_mensaje 		= ls_mensaje + "~nNombre"
					ls_colu[li_cont]	= "dere_nombre"
				End If				
			End If
		 End If
   Next
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If

FOR ll_Fila	= 1 TO dw_1.RowCount()
	dw_1.Object.espe_codigo[ll_Fila] = uo_SelEspecie.Codigo
NEXT
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_destinorechazado
integer x = 73
integer y = 364
integer width = 1687
integer height = 1356
integer taborder = 40
string dataobject = "dw_mant_mues_destinorechazado"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::itemchanged;call super::itemchanged;Integer	li_Null
String		ls_Columna 

SetNull(li_Null)
ls_Columna  = dwo.Name

Choose Case ls_Columna 
	Case "dere_codigo"
		If Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If

End Choose 


end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

RETURN 0
end event

event dw_1::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::getfocus;//
end event

event dw_1::sqlpreview;//
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_destinorechazado
integer x = 73
integer width = 1687
integer height = 276
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_destinorechazado
integer x = 2011
integer y = 144
integer width = 283
integer taborder = 20
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_destinorechazado
integer x = 2007
integer y = 440
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;uo_SelEspecie.Bloquear(False)
uo_SelEspecie.LimpiarDatos()

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled	= False
pb_imprimir.Enabled	= False

il_fila						= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_destinorechazado
integer x = 2007
integer y = 620
integer taborder = 30
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_destinorechazado
integer x = 2007
integer y = 800
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_destinorechazado
integer x = 2007
integer y = 980
integer taborder = 50
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_destinorechazado
integer x = 2007
integer y = 1160
integer taborder = 60
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_destinorechazado
integer x = 1998
integer y = 1544
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_destinorechazado
integer x = 315
integer y = 164
integer width = 288
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
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_destinorechazado
event destroy ( )
integer x = 585
integer y = 156
integer height = 80
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

