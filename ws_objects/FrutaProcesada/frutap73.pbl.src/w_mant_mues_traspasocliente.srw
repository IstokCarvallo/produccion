$PBExportHeader$w_mant_mues_traspasocliente.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_traspasocliente from w_mant_directo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_traspasocliente
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_traspasocliente
end type
type st_1 from statictext within w_mant_mues_traspasocliente
end type
type st_2 from statictext within w_mant_mues_traspasocliente
end type
type uo_selclientenew from uo_seleccion_clientesprod within w_mant_mues_traspasocliente
end type
type st_3 from statictext within w_mant_mues_traspasocliente
end type
type pb_todos from picturebutton within w_mant_mues_traspasocliente
end type
type pb_ninguno from picturebutton within w_mant_mues_traspasocliente
end type
end forward

global type w_mant_mues_traspasocliente from w_mant_directo
integer width = 4347
integer height = 1972
string title = "MANTENCIÓN ALTURA PALLET"
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_1 st_1
st_2 st_2
uo_selclientenew uo_selclientenew
st_3 st_3
pb_todos pb_todos
pb_ninguno pb_ninguno
end type
global w_mant_mues_traspasocliente w_mant_mues_traspasocliente

type variables

end variables

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)

	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		il_fila						= 1
		pb_grabar.Enabled	= True
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_traspasocliente.create
int iCurrent
call super::create
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_1=create st_1
this.st_2=create st_2
this.uo_selclientenew=create uo_selclientenew
this.st_3=create st_3
this.pb_todos=create pb_todos
this.pb_ninguno=create pb_ninguno
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selcliente
this.Control[iCurrent+2]=this.uo_selplanta
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.uo_selclientenew
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.pb_todos
this.Control[iCurrent+8]=this.pb_ninguno
end on

on w_mant_mues_traspasocliente.destroy
call super::destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selclientenew)
destroy(this.st_3)
destroy(this.pb_todos)
destroy(this.pb_ninguno)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelClienteNew.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelClienteNew.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	uo_SelPlanta.Inicia(gi_CodPlanta)
End If
end event

event ue_guardar;String	ls_Usuario, ls_Computador
Long	ll_Cliente, ll_Numero, ll_Planta, ll_ClienteNew, ll_Fila

SetPointer(HourGlass!)

If IsNull(uo_SelClienteNew.Codigo) Or uo_SelClienteNew.Codigo = -1 Then 
	MessageBox('Atencion', 'Debe Ingresar Cliente destino, para recepcion autoamtica.', Information!, Ok!)
	Return 
End If

ls_usuario 		= gstr_us.Nombre			
ls_computador 	= gstr_us.Computador	
ll_Cliente			= uo_SelCliente.Codigo
ll_ClienteNew	= uo_SelClienteNew.Codigo
ll_Planta			= uo_SelPlanta.Codigo

DECLARE Traspaso_Cliente PROCEDURE FOR dbo.FProc_TraspasoCliente_RecepcionElimina	
		  @cliente 		= :ll_Cliente,
		  @planta 		= :ll_Planta,
		  @numero 		= :ll_Numero,
		  @usuario		= :ls_Usuario, 
		  @computador	= :ls_Computador,
		  @ClienteNew = :ll_ClienteNew
USING SQLCA;

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		ll_numero		= dw_1.Object.rfpe_numero[ll_Fila]
		 
		EXECUTE Traspaso_Cliente;
					
		If Sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, "Lectura SP Traspaso Cliente. Debera Crear Recepcion de Forma Manual.")
		Else
			MessageBox('Atencion...', 'Se creo recepcion para Cliente:' + String(ll_ClienteNew, '000' + '~n~nRecepcion Original Nro: ' + &
																String(ll_Numero, '00000000')), Information!, Ok!)
		End If
		
		Close Traspaso_Cliente;
	End If
Next

TriggerEvent('ue_recuperadatos')

SetPointer(Arrow!)

end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_traspasocliente
integer x = 73
integer y = 44
integer width = 3310
integer height = 364
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_traspasocliente
boolean visible = false
integer x = 3890
integer y = 348
integer taborder = 60
boolean enabled = false
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

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_traspasocliente
integer x = 3890
integer y = 76
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_traspasocliente
boolean visible = false
integer x = 3890
integer y = 732
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_traspasocliente
boolean visible = false
integer x = 3890
integer y = 552
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_traspasocliente
integer x = 3890
integer y = 1476
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_traspasocliente
boolean visible = false
integer x = 3890
integer y = 1092
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_traspasocliente
integer x = 3890
integer y = 912
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_traspasocliente
integer x = 73
integer y = 456
integer width = 3310
integer height = 1292
integer taborder = 70
string dataobject = "dw_mues_recfruprocee"
boolean hscrollbar = true
end type

event dw_1::itemfocuschanged;call super::itemfocuschanged;CHOOSE CASE dwo.Name

	CASE "copa_nombre"
			pb_grabar.Enabled	=	True


END CHOOSE

end event

event dw_1::clicked;String	ls_old_sort, ls_column
Char	lc_sort

IF IsNull(dwo) THEN RETURN

If Right(dwo.Name,2) = '_t' Then
	ls_column = Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort = This.Describe("Datawindow.Table.sort")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + String(Rgb(255,255,255)))
	End If
	
	This.Modify(dwo.Name + ".Color = " + String(Rgb(255, 255, 0)))
	
	This.Sort()
End If

IF Row > 0 THEN
	This.SetRow(Row)
	If This.IsSelected(Row) Then 
		This.SelectRow(Row,False)
	Else
		This.SelectRow(Row,True)
	End If
END If
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_traspasocliente
integer x = 544
integer y = 80
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_traspasocliente
integer x = 544
integer y = 240
integer height = 100
integer taborder = 60
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_1 from statictext within w_mant_mues_traspasocliente
integer x = 165
integer y = 96
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_traspasocliente
integer x = 165
integer y = 256
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selclientenew from uo_seleccion_clientesprod within w_mant_mues_traspasocliente
integer x = 1623
integer y = 240
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selclientenew.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_mant_mues_traspasocliente
integer x = 1623
integer y = 96
integer width = 585
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Cliente a Traspasar"
boolean focusrectangle = false
end type

type pb_todos from picturebutton within w_mant_mues_traspasocliente
integer x = 2661
integer y = 72
integer width = 507
integer height = 144
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_todos_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_todos_off.png"
alignment htextalign = left!
end type

event clicked;Long ll_Fila 

For ll_Fila = 1 To dw_1.RowCount()
	dw_1.SelectRow(ll_Fila, True)
Next
end event

type pb_ninguno from picturebutton within w_mant_mues_traspasocliente
integer x = 2656
integer y = 232
integer width = 507
integer height = 144
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_off.png"
alignment htextalign = left!
end type

event clicked;dw_1.SelectRow(0, False)
end event

