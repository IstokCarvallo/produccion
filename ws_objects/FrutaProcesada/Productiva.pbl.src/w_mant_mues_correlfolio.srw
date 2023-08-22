$PBExportHeader$w_mant_mues_correlfolio.srw
$PBExportComments$Mantenedor de Técnicos
forward
global type w_mant_mues_correlfolio from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_correlfolio
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_correlfolio
end type
type pb_actualiza from picturebutton within w_mant_mues_correlfolio
end type
end forward

global type w_mant_mues_correlfolio from w_mant_tabla
integer width = 3799
integer height = 2344
string title = "MAESTRO CORRELATIVOS FOLIO"
event ue_validaregistro ( )
st_1 st_1
uo_selcliente uo_selcliente
pb_actualiza pb_actualiza
end type
global w_mant_mues_correlfolio w_mant_mues_correlfolio

type variables
uo_PlantaDesp	iuo_Planta
uo_grabatablas	iuo_grabatablas




end variables

forward prototypes
public function boolean duplicado (string campo, string valor)
public function boolean valida_rango (long rango, long fila)
public subroutine wf_replicacion ()
end prototypes

public function boolean duplicado (string campo, string valor);Long		ll_fila, ll_Numero
Integer	li_planta, li_Tipo
Boolean	lb_Retorno = False

li_Tipo		=	dw_1.Object.paen_tipopa[il_fila]
li_planta		=	dw_1.Object.plde_codigo[il_fila]
ll_Numero	=	dw_1.Object.spco_corini[il_fila]

CHOOSE CASE campo
	Case 'paen_tipopa'
		li_Tipo		=	Integer(valor)
		
	CASE "plde_Codigo"
		li_planta		=	Integer(valor)
		
	CASE "spco_corini"
		ll_Numero		=	Long(valor)	
	
END CHOOSE

ll_fila	= dw_1.Find("plde_codigo = " + String(li_planta) + ' And paen_tipopa = ' + String(li_Tipo) + &
							" and spco_corini = " + String(ll_Numero),1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	lb_Retorno = True
END IF

Return lb_Retorno
end function

public function boolean valida_rango (long rango, long fila);Long		ll_Fila, ll_Rango1, ll_Rango2
Boolean	lb_Retorno = False

For ll_fila = 1 To dw_1.Rowcount()
	ll_Rango1 = dw_1.Object.spco_corini[ll_Fila]
	ll_Rango2 = dw_1.Object.spco_corter[ll_Fila]
	
	If (rango >= ll_rango1 AND rango <= ll_rango2) And (ll_Fila <> Fila) Then
		MessageBox("Atención","Rango se encuentra contenido en otro registro. Ingrese Otro.", Exclamation!, Ok!)
		lb_Retorno =	True
	End If
Next

Return lb_Retorno
end function

public subroutine wf_replicacion ();Integer	ll_fila

ll_fila = dw_1.GetNextModIfied(ll_fila, Primary!)

If ll_fila > 0 Then
	If iuo_grabatablas.existereplicatablas(gi_CodExport) AND gstr_apl.CodigoSistema = 23 Then
		iuo_grabatablas.replicatabla_correlfolio(dw_1)	
	End If	
End If	
end subroutine

on w_mant_mues_correlfolio.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selcliente=create uo_selcliente
this.pb_actualiza=create pb_actualiza
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selcliente
this.Control[iCurrent+3]=this.pb_actualiza
end on

on w_mant_mues_correlfolio.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selcliente)
destroy(this.pb_actualiza)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Planta = Create uo_PlantaDesp
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	iuo_grabatablas		=	CREATE uo_grabatablas

	If gstr_apl.CodigoSistema <> 23 Then
		If gi_codexport = gi_cliebase Then
			istr_mant.Solo_Consulta = True
		End If
	End If

	buscar	=	"Número:fape_numero"
	ordenar	=	"Número:fape_numero"
End  If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta

DO
	ll_fila		=	dw_1.Retrieve(uo_SelCliente.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		pb_actualiza.Enabled	= True
	ELSE
		pb_insertar.Enabled	= True
		pb_actualiza.Enabled	= True
		pb_insertar.SetFocus()
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

lstr_info.titulo	= "MAESTRO CORRELATIVOS FOLIOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_correlfolio"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("Cliente.text = '" + uo_SelCliente.Nombre + "'")	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
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
	pb_grabar.Enabled	= True
END IF

il_fila = dw_1.InsertRow(0)

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn("plde_codigo")

istr_mant.Borra		= False
istr_mant.Agrega	= True

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled	= TRUE
END IF

dw_1.SetRow(il_fila)
end event

event ue_antesguardar;Long	ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]

IF dw_1.RowCount() > 0 THEN
   FOR ll_fila = 1 TO dw_1.RowCount()
		 IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
			 dw_1.DeleteRow(ll_fila)
			 ll_fila --
		 END IF
   NEXT
END IF

FOR ll_Fila	= 1 TO dw_1.RowCount()
	IF Isnull(dw_1.Object.plde_codigo[ll_fila]) OR dw_1.Object.plde_codigo[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPlanta"
		ls_colu[li_cont]	= "plde_codigo"
	END IF
	
	IF Isnull(dw_1.Object.spco_corini[ll_fila]) OR dw_1.Object.spco_corini[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCorrelativo Inicio"
		ls_colu[li_cont]	= "spco_corini"
	END IF
	
	IF Isnull(dw_1.Object.spco_corter[ll_fila]) OR dw_1.Object.spco_corter[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCorrelativo Término"
		ls_colu[li_cont]	= "spco_corter"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
		Return
	END IF
	
	dw_1.Object.clie_codigo[ll_Fila] = uo_SelCliente.Codigo
NEXT
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

event resize;call super::resize;Integer		li_posic_x, li_posic_y, li_visible, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_1.Resize(This.WorkSpaceWidth() - 490,This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x					=	78
st_encabe.width		=	dw_1.width

If st_encabe.Visible Then
	li_posic_y				=	st_encabe.y
Else
	li_posic_y				=	dw_1.y
End If

li_posic_x				=	This.WorkSpaceWidth() - 370

pb_lectura.x				=	li_posic_x
pb_lectura.y				=	li_posic_y
pb_lectura.width		=	li_Ancho
pb_lectura.height		=	li_Alto
li_posic_y 				+= li_Siguiente * 1.25

If pb_nuevo.Visible Then
	pb_nuevo.x			=	li_posic_x
	pb_nuevo.y			=	li_posic_y
	pb_nuevo.width	=	li_Ancho
	pb_nuevo.height	=	li_Alto
	li_visible++
	li_posic_y 			+= li_Siguiente
End If

If pb_insertar.Visible Then
	pb_insertar.x		=	li_posic_x
	pb_insertar.y		=	li_posic_y
	pb_insertar.width	=	li_Ancho
	pb_insertar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_eliminar.Visible Then
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_grabar.Visible Then
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_imprimir.Visible Then
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_actualiza.Visible Then
	pb_actualiza.x			=	li_posic_x
	pb_actualiza.y			=	li_posic_y
	pb_actualiza.width		=	li_Ancho
	pb_actualiza.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_correlfolio
integer x = 73
integer y = 364
integer width = 3067
integer height = 1356
integer taborder = 40
string dataobject = "dw_mues_correlfolio"
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

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "plde_codigo"
		If Not iuo_Planta.Existe(Integer(Data), True, Sqlca) Or Duplicado(ls_Columna, Data) Then 
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case "spco_corini"
		If Duplicado(ls_Columna, Data) Or valida_rango(Long(Data), Row)  Then
			This.SetItem(Row, ls_Columna, Long(li_Null))
			Return 1
		Else
			This.Object.spco_ultcor[Row] = Long(Data) - 1
		End If
		
	Case "spco_corter"	
		If Long(Data) < This.Object.spco_corini[Row] Then
			MessageBox("Atención","Correlativo Término No Puede ser Menor a Inicio. Ingrese Otra.", Exclamation!, Ok!)
			This.SetItem(Row, ls_Columna, Long(li_Null))
			Return 1
		End If
		
		If valida_rango(Long(Data), Row) Then
			This.SetItem(Row, ls_Columna, Long(li_Null))
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

event dw_1::itemerror;Return 1
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_correlfolio
integer x = 73
integer width = 3067
integer height = 276
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_correlfolio
integer x = 3374
integer y = 132
integer taborder = 20
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)


end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_correlfolio
integer x = 3369
integer y = 432
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled	= False
pb_imprimir.Enabled	= False
pb_actualiza.Enabled	= False

il_fila						= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_correlfolio
integer x = 3369
integer y = 684
integer taborder = 30
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_correlfolio
integer x = 3369
integer y = 936
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_correlfolio
integer x = 3369
integer y = 1192
integer taborder = 50
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_correlfolio
integer x = 3369
integer y = 1444
integer taborder = 60
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_correlfolio
integer x = 3360
integer y = 1972
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_correlfolio
integer x = 558
integer y = 164
integer width = 315
integer height = 96
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

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_correlfolio
event destroy ( )
integer x = 905
integer y = 164
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type pb_actualiza from picturebutton within w_mant_mues_correlfolio
integer x = 3355
integer y = 1700
integer width = 302
integer height = 244
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Actualizar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Actualizar-bn.png"
alignment htextalign = left!
end type

event clicked;uo_controlventanas	luo_Folios

luo_Folios	=	Create uo_controlventanas

luo_Folios.of_Actualiza(uo_SelCliente.Codigo, gi_CodPlanta, 1, dw_1)
luo_Folios.of_Actualiza(uo_SelCliente.Codigo, gi_CodPlanta, 2, dw_1)

Parent.TriggerEvent('ue_guardar')

Destroy luo_Folios
end event

