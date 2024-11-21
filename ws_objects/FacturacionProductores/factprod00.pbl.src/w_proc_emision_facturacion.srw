$PBExportHeader$w_proc_emision_facturacion.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_proc_emision_facturacion from w_para_informes
end type
type st_4 from statictext within w_proc_emision_facturacion
end type
type st_1 from statictext within w_proc_emision_facturacion
end type
type st_2 from statictext within w_proc_emision_facturacion
end type
type em_fecha from editmask within w_proc_emision_facturacion
end type
type st_6 from statictext within w_proc_emision_facturacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_proc_emision_facturacion
end type
type uo_selplanta from uo_seleccion_plantas within w_proc_emision_facturacion
end type
type dw_1 from uo_dw within w_proc_emision_facturacion
end type
type pb_todos from picturebutton within w_proc_emision_facturacion
end type
type pb_ninguno from picturebutton within w_proc_emision_facturacion
end type
type dw_proforma from uo_dw within w_proc_emision_facturacion
end type
end forward

global type w_proc_emision_facturacion from w_para_informes
integer x = 14
integer y = 32
integer width = 4443
integer height = 2232
string title = "Facturación Mensual de Productores"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
windowstate windowstate = maximized!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_fecha em_fecha
st_6 st_6
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
dw_1 dw_1
pb_todos pb_todos
pb_ninguno pb_ninguno
dw_proforma dw_proforma
end type
global w_proc_emision_facturacion w_proc_emision_facturacion

type variables

end variables

on w_proc_emision_facturacion.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_fecha=create em_fecha
this.st_6=create st_6
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.dw_1=create dw_1
this.pb_todos=create pb_todos
this.pb_ninguno=create pb_ninguno
this.dw_proforma=create dw_proforma
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.uo_selcliente
this.Control[iCurrent+7]=this.uo_selplanta
this.Control[iCurrent+8]=this.dw_1
this.Control[iCurrent+9]=this.pb_todos
this.Control[iCurrent+10]=this.pb_ninguno
this.Control[iCurrent+11]=this.dw_proforma
end on

on w_proc_emision_facturacion.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.st_6)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.dw_1)
destroy(this.pb_todos)
destroy(this.pb_ninguno)
destroy(this.dw_proforma)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	dw_1.SetTransObject(Sqlca)
	dw_proforma.SetTransObject(Sqlca)
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	em_Fecha.Text = String(Today(), 'mm/yyyy')
End If
end event

event resize;call super::resize;dw_1.Resize(This.WorkSpaceWidth() - 490,This.WorkSpaceHeight() - dw_1.y - 75)
end event

type pb_excel from w_para_informes`pb_excel within w_proc_emision_facturacion
integer x = 4059
integer y = 456
integer taborder = 20
end type

type st_computador from w_para_informes`st_computador within w_proc_emision_facturacion
end type

type st_usuario from w_para_informes`st_usuario within w_proc_emision_facturacion
end type

type st_temporada from w_para_informes`st_temporada within w_proc_emision_facturacion
end type

type p_logo from w_para_informes`p_logo within w_proc_emision_facturacion
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_proc_emision_facturacion
integer x = 247
integer width = 3168
string text = "Estado Factura Productores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_proc_emision_facturacion
integer x = 4082
integer y = 812
integer taborder = 90
integer weight = 400
fontcharset fontcharset = ansi!
boolean enabled = false
boolean default = false
string picturename = "\Desarrollo 22\Imagenes\Botones\Email.png"
string disabledname = "\Desarrollo 22\Imagenes\Botones\Email-bn.png"
end type

event pb_acepta::clicked;call super::clicked;Long 					ll_Fila, ll_Cliente, ll_Planta, ll_Productor, ll_Zona
Integer				li_Secuencia, li_Resultado
String					ls_Path, ls_Periodo, ls_Archivo, ls_Pass, ls_Asunto,ls_Texto, ls_Texto1, ls_Envio, ls_Error="", &
						ls_CC[] = {'jose.perez@rioblanco.net', 'daniela.oliva@rioblanco.net', 'francisca.gutierrez@rioblanco.net', &
						 				'Omar.Bravo@rioblanco.net', 'Roberto.Valderrama@rioblanco.net', 'Susana.Sagardia@rioblanco.net'}, & 
						ls_Mes[] = {'Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre'}
Date					ld_Desde, ld_Hasta, ld_Fecha
Dec{2}				ld_Cambio

uo_Especies				luo_Especie 
uo_Productores			luo_Productor
uo_Mail					luo_Mail
uo_facturaproductor	luo_Factura

If dw_1.RowCount() = 0 Then Return

luo_Productor	=	Create uo_Productores
luo_Especie		=	Create uo_Especies
luo_Factura		=	Create uo_facturaproductor
luo_Mail			=	Create uo_Mail

ls_Path = 'C:\PdfBck'

ls_Periodo	= Mid(em_fecha.Text, 4) + Mid(em_fecha.Text, 1, 2) 

If Not DirectoryExists (ls_path) Then CreateDirectory (ls_path)
If Not DirectoryExists (ls_path + '\' + ls_Periodo) Then CreateDirectory (ls_path + '\' + ls_Periodo)

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		ll_Cliente		=	dw_1.Object.clie_codigo[ll_Fila]
		ll_Planta		=	dw_1.Object.plde_codigo[ll_Fila]
		ll_Productor	=	dw_1.Object.prod_codigo[ll_Fila]
		ll_Zona		=	dw_1.Object.zona_codigo[ll_Fila]
		li_Secuencia=	dw_1.Object.faen_secuen[ll_Fila]
		ld_Desde		=	Date(dw_1.Object.faen_fecini[ll_Fila])
		ld_Hasta 		=	Date(dw_1.Object.faen_fecter[ll_Fila])
		ld_Cambio	=	dw_1.Object.faen_cambio[ll_Fila]
		
		dw_Proforma.Reset()
		ld_Fecha = Date('01/' + em_Fecha.Text)
		If dw_Proforma.Retrieve(ll_Cliente, ll_Planta, Date('01/' + em_Fecha.Text), ll_Productor, ll_Zona, &
											ld_Cambio, 19, 0, 0, 0, 0, ld_Desde, ld_Hasta, -1, li_Secuencia) < 1 Then 
				ls_Error = ls_Error + '[' + String(ll_Productor, '00000') + '] No esta disponible Proforma.~n'
		Else
			
			luo_Productor.Existe(ll_Productor, True, SQLCA)
				
			dw_Proforma.Object.DataWindow.Export.PDF.NativePDF.UsePrintSpec		= 'Yes'
			dw_Proforma.Object.DataWindow.Export.PDF.Method							= NativePDF!
			dw_Proforma.Object.DataWindow.Export.PDF.NativePDF.ImageFormat		= '1'
			dw_Proforma.Object.DataWindow.Export.PDF.NativePDF.CustomSize			= '5'
			dw_Proforma.Object.DataWindow.Export.PDF.NativePDF.Restrictions			= 'noannots!'
			dw_Proforma.Object.DataWindow.Export.PDF.NativePDF.MasterPassword	= '6460'
								
			ls_Archivo = ls_Periodo + '_' + String(luo_Productor.Codigo, '00000') + '.PDF'
			ls_Pass = String(Long(Mid(luo_Productor.Rut,1,9)))
		
			dw_Proforma.Object.DataWindow.Export.PDF.NativePDF.UserPassword	=	ls_Pass
			li_Resultado = dw_Proforma.SaveAs(ls_path + '\' + ls_Periodo + '\' + ls_Archivo, PDF!, True)
			
			If li_Resultado = 1 Then
				If dw_1.Object.faen_enviado[ll_Fila] = 0 Then
					If UpperBound(luo_Productor.Mail) > 0 And luo_Productor.Mail[1] <> '' Then 
						
						luo_Especie.Existe(dw_1.Object.espe_codigo[ll_Fila], False, SQLCA)
						
						ls_Envio		=	ls_path + '\' + ls_Periodo + '\' + ls_Archivo
						ls_Asunto	=	'Facturación '+ luo_Especie.Nombre + ' Exportación ' + em_Fecha.Text + ' Prod. ' + String(luo_Productor.Codigo, '00000') + ' ' + luo_Productor.Nombre
						
						ls_Texto		=	'Estimado productor buenos dias,~n~n'
						ls_Texto		+=	'Junto con saludar, solicitamos a Ud. emitir factutación del mes de ' + ls_Mes[Integer(Mid(em_fecha.Text, 1, 2))] + &
														'-' + Mid(em_fecha.Text, 4) + ' con el siguiente detalle:.~n~n'
						ls_Texto		+=	'Factura por fruta de exportación.~n'
						ls_Texto		+=	'Nombre: ' + uo_SelCliente.Nombre + '~n'
						ls_Texto		+=	'Rut: ' + String(Long(Mid(uo_SelCliente.Rut, 1, 9)), '#,##0') + '-' + Right(uo_SelCliente.Rut, 1)+ ' ~n'
						ls_Texto		+=	'Dirección: ' + uo_SelCliente.DireccionCompleta +' ~n'
						ls_Texto		+=	'Giro: ' + uo_SelCliente.Giro +' ~n'
						
						luo_Factura.Of_Totales(ll_Cliente, ll_Planta, ll_Zona, ll_Productor, Date('01/' + em_Fecha.Text), li_Secuencia, 0, SQLCA)
						
						ls_Texto		+= '~n~n Con PMG~n~n'
						ls_Texto		+=	'Glosa: ' + String(luo_Factura.Kilos, '#,##0.00') + ' Kilos de ' + luo_Especie.Nombre + ' de exportacion, '
						If Not IsNull(luo_Factura.GGN) And luo_Factura.GGN <> '' Then  ls_Texto	+=	'Certificada Global GAP ' + luo_Factura.GGN
						ls_Texto		+=	' variedad ' + luo_Factura.Variedad + 'de acuerdo con detalle adjunto.~n'
						
						ls_Texto		+=	'Neto: ' + String(luo_Factura.Neto, '#,##0') + ' ~n'
						ls_Texto		+=	'IVA: ' + String(luo_Factura.IVA, '#,##0') + ' ~n'
						ls_Texto		+=	'Total: ' + String(luo_Factura.Total, '#,##0') + ' ~n'
					
						luo_Factura.Of_Totales(ll_Cliente, ll_Planta, ll_Zona, ll_Productor, Date('01/' + em_Fecha.Text), li_Secuencia, 1, SQLCA)
						
						If Not IsNull(luo_Factura.Kilos) Then
							ls_Texto		+= ' ~n SIN PMG~n~n'
							ls_Texto	+=	'Glosa: ' + String(luo_Factura.Kilos, '#,##0.00') + ' Kilos de ' + luo_Especie.Nombre + ' de exportacion, '
							If Not IsNull(luo_Factura.GGN) And luo_Factura.GGN <> '' Then  ls_Texto	+=	'Certificada Global GAP ' + luo_Factura.GGN
							ls_Texto	+=	' variedad ' + luo_Factura.Variedad + 'de acuerdo con detalle adjunto.~n'
							
							ls_Texto	+=	'Neto: ' + String(luo_Factura.Neto, '#,##0') + ' ~n'
							ls_Texto	+=	'IVA: ' + String(luo_Factura.IVA, '#,##0') + ' ~n'
							ls_Texto	+=	'Total: ' + String(luo_Factura.Total, '#,##0') + ' ~n'
						End If
						
						ls_Texto		+=	 '~nNota:Por seguridad, para abrir el archivo deberás ingresar una clave que corresponde a los dígitos de tu Rut (sin dígito verificador).'
						ls_Texto		+=	 '~n~tRecordar facturar con fecha ' + luo_Factura.of_UltimoDia(em_fecha.Text)
						
						luo_Mail.Of_Send(luo_Productor.Mail, ls_CC, ls_Asunto, ls_Texto, {ls_Envio}, 0) 
						
						If Not DirectoryExists (ls_path + '\' + ls_Periodo + '\Enviado') Then CreateDirectory (ls_path + '\' + ls_Periodo + '\Enviado')
						
						If FileMove (ls_path + '\' + ls_Periodo + '\' + ls_Archivo,  ls_path + '\' + ls_Periodo + '\Enviado\' + ls_Archivo) = -1 Then 
							ls_Error = ls_Error + '[' + String(ll_Productor, '00000') + '] No se puedo mover el archivo.~n'
						Else
							
							Update dbo.facturprodenca
								Set  faen_enviado = 1
								Where clie_codigo = :ll_Cliente
									And plde_codigo = :ll_Planta
									And prod_codigo = :luo_Productor.Codigo
									And zona_codigo = :ll_zona
									And Datediff(mm, faen_fechaf, :ld_Fecha) = 0
								Using SQLCA;
							
								If SQLCA.SQLCode = -1 Then
									ls_Error = ls_Error + '[' + String(ll_Productor, '00000') + '] No pudo actualizar estado de envio.~n'								
								End If							
						End If
					Else
						ls_Error = ls_Error + '[' + String(ll_Productor, '00000') + '] No tiene email disponible para envio.~n'
					End If
				End If
			Else
				ls_Error = ls_Error + '[' + String(ll_Productor, '00000') + '] No se pudo generar el archivo.~n'
			End If
		End If
	End If
Next

If Not IsNull(ls_Error) And ls_Error <> "" Then MessageBox('Informacion', ls_Error, Information!, OK!)

dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, 1, Date('01/' + em_fecha.Text))

Destroy luo_Productor
Destroy luo_Especie
Destroy luo_Factura
Destroy luo_Mail

end event

type pb_salir from w_para_informes`pb_salir within w_proc_emision_facturacion
integer x = 4087
integer y = 1132
integer taborder = 100
end type

type st_4 from statictext within w_proc_emision_facturacion
integer x = 247
integer y = 424
integer width = 3168
integer height = 316
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_proc_emision_facturacion
integer x = 1568
integer y = 604
integer width = 219
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_proc_emision_facturacion
integer x = 343
integer y = 608
integer width = 485
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mes de Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_proc_emision_facturacion
integer x = 850
integer y = 592
integer width = 311
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

event modified;
If IsNull(This.Text) Then Return

If dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, 1, Date('01/' + This.Text)) > 0 Then Parent.pb_acepta.Enabled = True
end event

type st_6 from statictext within w_proc_emision_facturacion
integer x = 343
integer y = 492
integer width = 233
integer height = 64
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

type uo_selcliente from uo_seleccion_clientesprod within w_proc_emision_facturacion
event destroy ( )
integer x = 581
integer y = 480
integer height = 92
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_proc_emision_facturacion
event destroy ( )
integer x = 1806
integer y = 500
integer height = 188
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_1 from uo_dw within w_proc_emision_facturacion
integer x = 64
integer y = 772
integer width = 3831
integer height = 1348
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_estadoproforma_envio"
boolean hscrollbar = true
boolean border = false
end type

event clicked;call super::clicked;Long	ll_Fila, ll_SelectedRow

If Row = 0 Then
	/*
	Para que funcione este ordenamiento los títulos deben tener el nombre
	de la columna y terminacion "_t", de lo contrario no funcionará
	*/
	String		ls_old_sort, ls_column, ls_color_old
	Char		lc_sort
	
	IF IsNull(dwo) THEN RETURN
	
	If Right(dwo.Name,2) = '_t' Then
		ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
		ls_old_sort	= This.Describe("Datawindow.Table.sort")
		ls_color_old	=This.Describe(ls_Column + "_t.Color")
	
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
			This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
		End If
		
		This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
		
		This.Sort()
	End If

Else
	ll_SelectedRow = This.GetSelectedRow(0)
	
	If KeyDown(keyShIft!) Then
		If ll_SelectedRow = 0 Then
			This.SelectRow(Row, True)
		Else
			This.SelectRow(1, False)
			If Row > ll_SelectedRow Then
				For ll_Fila = ll_SelectedRow To Row
					This.SelectRow(ll_Fila, True)
				Next
			Else
				For ll_Fila = Row To ll_SelectedRow
					This.SelectRow(ll_Fila, True)
				Next
			End If
		End If
	
	ElseIf KeyDown(keyControl!) Then
		If This.IsSelected(Row) Then
			This.SelectRow(Row, False)
		Else
			This.SelectRow(Row, True)
		End If
	
	Else
		If This.IsSelected(Row) Then
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		Else
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		End If
	End If
End If
end event

type pb_todos from picturebutton within w_proc_emision_facturacion
integer x = 2848
integer y = 440
integer width = 549
integer height = 140
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_todos_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_todos_off.png"
alignment htextalign = right!
end type

event clicked;dw_1.SetRedraw(False)
dw_1.SelectRow(0,True)
dw_1.SetRedraw(True)
end event

type pb_ninguno from picturebutton within w_proc_emision_facturacion
integer x = 2848
integer y = 592
integer width = 549
integer height = 140
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_off.png"
alignment htextalign = left!
end type

event clicked;dw_1.SetRedraw(False)
dw_1.SelectRow(0,False)
dw_1.SetRedraw(True)
end event

type dw_proforma from uo_dw within w_proc_emision_facturacion
boolean visible = false
integer x = 3410
integer y = 16
integer width = 411
integer height = 272
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_info_facturacion_productorgeneradodet"
boolean vscrollbar = false
boolean border = false
end type

