$PBExportHeader$w_proc_estado_facturacion.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_proc_estado_facturacion from w_para_informes
end type
type st_4 from statictext within w_proc_estado_facturacion
end type
type st_1 from statictext within w_proc_estado_facturacion
end type
type st_2 from statictext within w_proc_estado_facturacion
end type
type em_fecha from editmask within w_proc_estado_facturacion
end type
type st_6 from statictext within w_proc_estado_facturacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_proc_estado_facturacion
end type
type uo_selplanta from uo_seleccion_plantas within w_proc_estado_facturacion
end type
type dw_1 from uo_dw within w_proc_estado_facturacion
end type
type pb_todos from picturebutton within w_proc_estado_facturacion
end type
type pb_ninguno from picturebutton within w_proc_estado_facturacion
end type
end forward

global type w_proc_estado_facturacion from w_para_informes
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
end type
global w_proc_estado_facturacion w_proc_estado_facturacion

type variables

end variables

on w_proc_estado_facturacion.create
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
end on

on w_proc_estado_facturacion.destroy
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
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	dw_1.SetTransObject(Sqlca)
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
//	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_Fecha.Text = String(Today(), 'mm/yyyy')
	
	dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, -1, Date('01/' + em_Fecha.Text))
End If
end event

event resize;call super::resize;dw_1.Resize(This.WorkSpaceWidth() - 490,This.WorkSpaceHeight() - dw_1.y - 75)
end event

type pb_excel from w_para_informes`pb_excel within w_proc_estado_facturacion
integer x = 4059
integer y = 456
integer taborder = 20
end type

type st_computador from w_para_informes`st_computador within w_proc_estado_facturacion
end type

type st_usuario from w_para_informes`st_usuario within w_proc_estado_facturacion
end type

type st_temporada from w_para_informes`st_temporada within w_proc_estado_facturacion
end type

type p_logo from w_para_informes`p_logo within w_proc_estado_facturacion
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_proc_estado_facturacion
integer x = 247
integer width = 3168
string text = "Estado Factura Productores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_proc_estado_facturacion
integer x = 4082
integer y = 812
integer taborder = 90
integer weight = 400
fontcharset fontcharset = ansi!
boolean default = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
end type

event pb_acepta::clicked;Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False


IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, PArent.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, Parent.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

end event

type pb_salir from w_para_informes`pb_salir within w_proc_estado_facturacion
integer x = 4087
integer y = 1132
integer taborder = 100
end type

type st_4 from statictext within w_proc_estado_facturacion
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

type st_1 from statictext within w_proc_estado_facturacion
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

type st_2 from statictext within w_proc_estado_facturacion
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

type em_fecha from editmask within w_proc_estado_facturacion
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

event modified;If IsNull(This.Text) Then Return

dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, -1, Date('01/' + This.Text))
end event

type st_6 from statictext within w_proc_estado_facturacion
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

type uo_selcliente from uo_seleccion_clientesprod within w_proc_estado_facturacion
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

type uo_selplanta from uo_seleccion_plantas within w_proc_estado_facturacion
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

type dw_1 from uo_dw within w_proc_estado_facturacion
integer x = 64
integer y = 772
integer width = 3831
integer height = 1348
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_estadoproforma"
boolean hscrollbar = true
boolean border = false
end type

event doubleclicked;call super::doubleclicked;If IsNull(Row) Or Row < 1 Then Return
string	ls_fecha

ls_Fecha = String(today(), 'yyyymmdd hh:mm:ss')

If This.Object.faen_estado[Row] = 0 Then
	This.Object.faen_estado[Row]	= 1
	This.Object.faen_usucre[Row]	= gstr_Us.Nombre
	This.Object.faen_fecval[Row]	= DateTime(ls_Fecha)
ElseIf This.Object.faen_estado[Row] = 1 Then
	This.Object.faen_estado[Row]	= 0
	This.Object.faen_usumod[Row]= gstr_Us.Nombre
	This.Object.faen_fecmod[Row]	= DateTime(ls_Fecha)
Else
	MessageBox('Atencion', 'Factura esta trasmitida a contabiliodad. Comunicarse con area contable.', Exclamation!, Ok!)
	Return
End If
end event

type pb_todos from picturebutton within w_proc_estado_facturacion
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

event clicked;Long	ll_Fila
String	ls_Fecha

ls_Fecha = String(today(), 'yyyymmdd hh:mm:ss')

For ll_Fila = 1 To dw_1.RowCount()
	dw_1.Object.faen_estado[ll_Fila]	= 1
	dw_1.Object.faen_usucre[ll_Fila]	= gstr_Us.Nombre
	dw_1.Object.faen_fecval[ll_Fila] 	=	Datetime(ls_Fecha)
Next
end event

type pb_ninguno from picturebutton within w_proc_estado_facturacion
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

event clicked;Long	ll_Fila
String	ls_Fecha

ls_Fecha = String(today(), 'yyyymmdd hh:mm:ss')

For ll_Fila = 1 To dw_1.RowCount()
	dw_1.Object.faen_estado[ll_Fila] = 0
	dw_1.Object.faen_usumod[ll_Fila] = gstr_Us.Nombre
	dw_1.Object.faen_fecmod[ll_Fila] = Datetime(ls_Fecha)
Next
end event

