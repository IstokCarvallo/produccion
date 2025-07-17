$PBExportHeader$w_info_palletcheep.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_palletcheep from w_para_informes
end type
type st_1 from statictext within w_info_palletcheep
end type
type st_6 from statictext within w_info_palletcheep
end type
type st_3 from statictext within w_info_palletcheep
end type
type st_variedad from statictext within w_info_palletcheep
end type
type st_5 from statictext within w_info_palletcheep
end type
type st_4 from statictext within w_info_palletcheep
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_palletcheep
end type
type uo_selplanta from uo_seleccion_plantas within w_info_palletcheep
end type
type uo_selembarcador from uo_seleccion_embarcadores within w_info_palletcheep
end type
type st_nave from statictext within w_info_palletcheep
end type
type cbx_embarque from checkbox within w_info_palletcheep
end type
type cbx_embarqueconsol from checkbox within w_info_palletcheep
end type
type cb_embarque from commandbutton within w_info_palletcheep
end type
type em_embarque from editmask within w_info_palletcheep
end type
type cbx_operacion from checkbox within w_info_palletcheep
end type
type cbx_operconsol from checkbox within w_info_palletcheep
end type
type em_operacion from editmask within w_info_palletcheep
end type
type rb_pallet from radiobutton within w_info_palletcheep
end type
type rb_ficha from radiobutton within w_info_palletcheep
end type
type st_2 from statictext within w_info_palletcheep
end type
end forward

global type w_info_palletcheep from w_para_informes
integer x = 14
integer y = 32
integer width = 2501
integer height = 1848
string title = "PLANIFICA LLEGADA DESDE PACKING"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_6 st_6
st_3 st_3
st_variedad st_variedad
st_5 st_5
st_4 st_4
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selembarcador uo_selembarcador
st_nave st_nave
cbx_embarque cbx_embarque
cbx_embarqueconsol cbx_embarqueconsol
cb_embarque cb_embarque
em_embarque em_embarque
cbx_operacion cbx_operacion
cbx_operconsol cbx_operconsol
em_operacion em_operacion
rb_pallet rb_pallet
rb_ficha rb_ficha
st_2 st_2
end type
global w_info_palletcheep w_info_palletcheep

type variables
uo_EmbarquesProd 	iuo_Embarques
end variables

on w_info_palletcheep.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_5=create st_5
this.st_4=create st_4
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selembarcador=create uo_selembarcador
this.st_nave=create st_nave
this.cbx_embarque=create cbx_embarque
this.cbx_embarqueconsol=create cbx_embarqueconsol
this.cb_embarque=create cb_embarque
this.em_embarque=create em_embarque
this.cbx_operacion=create cbx_operacion
this.cbx_operconsol=create cbx_operconsol
this.em_operacion=create em_operacion
this.rb_pallet=create rb_pallet
this.rb_ficha=create rb_ficha
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_variedad
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.uo_selcliente
this.Control[iCurrent+8]=this.uo_selplanta
this.Control[iCurrent+9]=this.uo_selembarcador
this.Control[iCurrent+10]=this.st_nave
this.Control[iCurrent+11]=this.cbx_embarque
this.Control[iCurrent+12]=this.cbx_embarqueconsol
this.Control[iCurrent+13]=this.cb_embarque
this.Control[iCurrent+14]=this.em_embarque
this.Control[iCurrent+15]=this.cbx_operacion
this.Control[iCurrent+16]=this.cbx_operconsol
this.Control[iCurrent+17]=this.em_operacion
this.Control[iCurrent+18]=this.rb_pallet
this.Control[iCurrent+19]=this.rb_ficha
this.Control[iCurrent+20]=this.st_2
end on

on w_info_palletcheep.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selembarcador)
destroy(this.st_nave)
destroy(this.cbx_embarque)
destroy(this.cbx_embarqueconsol)
destroy(this.cb_embarque)
destroy(this.em_embarque)
destroy(this.cbx_operacion)
destroy(this.cbx_operconsol)
destroy(this.em_operacion)
destroy(this.rb_pallet)
destroy(this.rb_ficha)
destroy(this.st_2)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar = True
If IsNull(uo_SelEmbarcador.Codigo) THEN lb_Cerrar = True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(False,False)
	uo_SelPlanta.Seleccion(True, True)
	uo_SelEmbarcador.Seleccion(True, True)
	
	uo_SelPlanta.Filtra(1)
	uo_SelCliente.Inicia(gi_CodExport)
	
	iuo_Embarques 	=	Create uo_EmbarquesProd
End If



end event

type pb_excel from w_para_informes`pb_excel within w_info_palletcheep
end type

type st_computador from w_para_informes`st_computador within w_info_palletcheep
end type

type st_usuario from w_para_informes`st_usuario within w_info_palletcheep
end type

type st_temporada from w_para_informes`st_temporada within w_info_palletcheep
end type

type p_logo from w_para_informes`p_logo within w_info_palletcheep
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_palletcheep
integer width = 1705
string text = "Palloet CHEEP"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_palletcheep
integer x = 2057
integer y = 784
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_Operacion
String		ls_Embarque

istr_info.titulo	= 'PALLET CHEEP'	

If cbx_operacion.Checked Then
	li_Operacion = -1
	If cbx_OperConsol.Checked Then
		li_Operacion = -9
	End If
Else
	li_Operacion = Integer(em_Operacion.Text)
End If

If cbx_Embarque.Checked Then
	ls_Embarque = '*'
	If cbx_EmbarqueConsol.Checked Then
		ls_Embarque = '**'
	End If
Else
	ls_Embarque = em_Embarque.Text
End If

OpenWithParm(vinf, istr_info)
If rb_Pallet.Checked Then
	vinf.dw_1.DataObject = "dw_info_palletcheep"
Else
	vinf.dw_1.DataObject = "dw_info_palletcheep_ficha"
End If

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, li_Operacion, ls_Embarque, uo_SelEmbarcador.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_palletcheep
integer x = 2062
integer y = 1052
integer taborder = 140
end type

type st_1 from statictext within w_info_palletcheep
integer x = 293
integer y = 720
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_palletcheep
integer x = 293
integer y = 496
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_palletcheep
integer x = 293
integer y = 1044
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embarque"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_palletcheep
integer x = 293
integer y = 1360
integer width = 366
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embarcador"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_palletcheep
integer x = 293
integer y = 864
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Operacion"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_palletcheep
integer x = 251
integer y = 440
integer width = 1705
integer height = 1088
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_palletcheep
event destroy ( )
integer x = 699
integer y = 484
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_palletcheep
event destroy ( )
integer x = 699
integer y = 624
integer height = 184
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selembarcador from uo_seleccion_embarcadores within w_info_palletcheep
integer x = 699
integer y = 1268
integer taborder = 150
boolean bringtotop = true
end type

on uo_selembarcador.destroy
call uo_seleccion_embarcadores::destroy
end on

type st_nave from statictext within w_info_palletcheep
integer x = 699
integer y = 1144
integer width = 1184
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cbx_embarque from checkbox within w_info_palletcheep
integer x = 1198
integer y = 1036
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
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Embarque.Enabled	= False
	cb_Embarque.Enabled	= False
	em_Operacion.Text		=	''
	em_Embarque.Text 		=	''
	st_Nave.Text				=	''
Else
	em_Embarque.Enabled	= True
	cb_Embarque.Enabled	= True
End If
end event

type cbx_embarqueconsol from checkbox within w_info_palletcheep
integer x = 1499
integer y = 1036
integer width = 384
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
string text = "Consolida"
end type

event clicked;If This.Checked Then
	em_Embarque.Enabled	=	False
	cb_Embarque.Enabled	=	False
	cbx_Embarque.Enabled	=	False
	cbx_Embarque.Checked	=	True
	em_Operacion.Text		=	''
	em_Embarque.Text 		=	''
	st_Nave.Text				=	''
Else
	cbx_Embarque.Enabled	=	True	
End If
end event

type cb_embarque from commandbutton within w_info_palletcheep
integer x = 1065
integer y = 1032
integer width = 114
integer height = 88
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1] = String(uo_selCliente.Codigo)
OpenWithParm(w_busc_embarques_consignatario, lstr_busq)

lstr_busq		= Message.PowerObjectParm

If lstr_busq.argum[1] = "" Then
	em_embarque.SetFocus()
Else
	If Not iuo_Embarques.Existe(lstr_busq.argum[1], True, Sqlca) Then
		em_Embarque.Text	=	''
		em_Embarque.SetFocus()
	Else
		em_Operacion.Text	=	String(iuo_Embarques.Operacion, '0000')
		st_nave.Text			=	iuo_Embarques.NombreNave
		em_Embarque.Text	=	iuo_Embarques.Codigo
		uo_SelEmbarcador.Todos(False)
		uo_SelEmbarcador.Inicia(iuo_Embarques.Embarcador)	
	End If
End If



end event

type em_embarque from editmask within w_info_palletcheep
integer x = 699
integer y = 1020
integer width = 343
integer height = 112
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;If IsNull(This.Text) Then Return

If Not iuo_Embarques.Existe(This.Text, True, Sqlca) Then
	This.Text	=	''
	Return
Else
	em_Operacion.Text	=	String(iuo_Embarques.Operacion, '0000')
	st_nave.Text			=	iuo_Embarques.NombreNave
	This.Text					=	iuo_Embarques.Codigo
	uo_SelEmbarcador.Todos(False)
	uo_SelEmbarcador.Inicia(iuo_Embarques.Embarcador)	
End If
end event

type cbx_operacion from checkbox within w_info_palletcheep
integer x = 1198
integer y = 868
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
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Operacion.Enabled	= False
	em_Operacion.Text 		=	''
	st_Nave.Text				=	''
Else
	em_Operacion.Enabled	= True
End If
end event

type cbx_operconsol from checkbox within w_info_palletcheep
integer x = 1499
integer y = 872
integer width = 384
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
string text = "Consolida"
end type

event clicked;If This.Checked Then
	em_Operacion.Enabled	=	False
	cbx_Operacion.Enabled	=	False
	cbx_Operacion.Checked	=	True
	em_Operacion.Text 		=	''
Else
	cbx_Operacion.Enabled	=	True	
End If
end event

type em_operacion from editmask within w_info_palletcheep
integer x = 699
integer y = 852
integer width = 343
integer height = 112
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;If IsNull(This.Text) Then Return

If Not iuo_Embarques.of_Operacion(Integer(This.Text), True, Sqlca) Then
	This.Text	=	''
	Return
Else
	This.Text					=	String(iuo_Embarques.Operacion, '0000')
	st_nave.Text			=	iuo_Embarques.NombreNave	
End If
end event

type rb_pallet from radiobutton within w_info_palletcheep
integer x = 603
integer y = 1564
integer width = 325
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
string text = "Pallet"
boolean checked = true
end type

type rb_ficha from radiobutton within w_info_palletcheep
integer x = 1202
integer y = 1564
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
string text = "Ficha"
end type

type st_2 from statictext within w_info_palletcheep
integer x = 251
integer y = 1524
integer width = 1705
integer height = 164
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

