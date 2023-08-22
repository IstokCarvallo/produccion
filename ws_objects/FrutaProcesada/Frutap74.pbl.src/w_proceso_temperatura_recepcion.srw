$PBExportHeader$w_proceso_temperatura_recepcion.srw
forward
global type w_proceso_temperatura_recepcion from w_para_informes
end type
type st_1 from statictext within w_proceso_temperatura_recepcion
end type
type st_2 from statictext within w_proceso_temperatura_recepcion
end type
type st_6 from statictext within w_proceso_temperatura_recepcion
end type
type st_5 from statictext within w_proceso_temperatura_recepcion
end type
type dw_1 from datawindow within w_proceso_temperatura_recepcion
end type
type pb_recupera from picturebutton within w_proceso_temperatura_recepcion
end type
type st_4 from statictext within w_proceso_temperatura_recepcion
end type
type em_recepcion from editmask within w_proceso_temperatura_recepcion
end type
type pb_1 from picturebutton within w_proceso_temperatura_recepcion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_proceso_temperatura_recepcion
end type
type uo_selplantas from uo_seleccion_plantas within w_proceso_temperatura_recepcion
end type
end forward

global type w_proceso_temperatura_recepcion from w_para_informes
integer x = 14
integer y = 32
integer width = 3872
integer height = 2016
string title = "TEMPERATURA PALLET RECEPCIONADOS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
st_6 st_6
st_5 st_5
dw_1 dw_1
pb_recupera pb_recupera
st_4 st_4
em_recepcion em_recepcion
pb_1 pb_1
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_proceso_temperatura_recepcion w_proceso_temperatura_recepcion

type variables

end variables

forward prototypes
public function boolean existerecepcion (long al_recepcion)
end prototypes

public function boolean existerecepcion (long al_recepcion);Integer		li_cont

SELECT	count(*)
	INTO	:li_cont
	FROM	dbo.recfruproced
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND plde_codigo = :uo_SelPlantas.Codigo
	AND rfpe_numero = :al_recepcion;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla recfruproced")
	
	RETURN True
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "NO Existe Recepción.~r~r" + &
					"Ingrese otro Número.")
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_proceso_temperatura_recepcion.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_6=create st_6
this.st_5=create st_5
this.dw_1=create dw_1
this.pb_recupera=create pb_recupera
this.st_4=create st_4
this.em_recepcion=create em_recepcion
this.pb_1=create pb_1
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.pb_recupera
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.em_recepcion
this.Control[iCurrent+9]=this.pb_1
this.Control[iCurrent+10]=this.uo_selcliente
this.Control[iCurrent+11]=this.uo_selplantas
end on

on w_proceso_temperatura_recepcion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.dw_1)
destroy(this.pb_recupera)
destroy(this.st_4)
destroy(this.em_recepcion)
destroy(this.pb_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_proceso_temperatura_recepcion
integer x = 3447
integer y = 44
end type

type st_computador from w_para_informes`st_computador within w_proceso_temperatura_recepcion
boolean visible = false
integer x = 3689
integer y = 1680
end type

type st_usuario from w_para_informes`st_usuario within w_proceso_temperatura_recepcion
boolean visible = false
integer x = 3689
integer y = 1608
end type

type st_temporada from w_para_informes`st_temporada within w_proceso_temperatura_recepcion
boolean visible = false
integer x = 3689
integer y = 1536
end type

type p_logo from w_para_informes`p_logo within w_proceso_temperatura_recepcion
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_proceso_temperatura_recepcion
integer x = 242
integer width = 3090
string text = "Temperatura de Pallet Recepcionados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_proceso_temperatura_recepcion
integer x = 3456
integer y = 872
integer taborder = 150
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
long backcolor = 553648127
end type

event pb_acepta::clicked;Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, w_proceso_temperatura_recepcion.Title)
		MessageBox("Problema", "Grabación NO Realizada.")
	ELSE
					
		dw_1.ResetUpdate()
		
		MessageBox("Atención", "Grabación Satisfactoria.")
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, w_proceso_temperatura_recepcion.Title)
	MessageBox("Problema", "Grabación NO Realizada.")
END IF

sqlca.AutoCommit	=	lb_AutoCommit


end event

type pb_salir from w_para_informes`pb_salir within w_proceso_temperatura_recepcion
integer x = 3451
integer y = 1372
integer taborder = 170
long backcolor = 553648127
end type

type st_1 from statictext within w_proceso_temperatura_recepcion
integer x = 389
integer y = 584
integer width = 293
integer height = 76
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

type st_2 from statictext within w_proceso_temperatura_recepcion
integer x = 389
integer y = 692
integer width = 402
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
string text = "N° Recepción"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_proceso_temperatura_recepcion
integer x = 389
integer y = 488
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
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_5 from statictext within w_proceso_temperatura_recepcion
integer x = 242
integer y = 812
integer width = 3090
integer height = 1044
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

type dw_1 from datawindow within w_proceso_temperatura_recepcion
integer x = 265
integer y = 824
integer width = 3045
integer height = 1008
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_proceso_mant_temperatura_recep"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_recupera from picturebutton within w_proceso_temperatura_recepcion
integer x = 3451
integer y = 588
integer width = 302
integer height = 244
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer 	fila
Long		ll_recepcion

pb_acepta.Enabled = False

ll_recepcion 	=	Long(em_recepcion.Text)

dw_1.SetTransObject(sqlca)

fila	=	dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,ll_recepcion)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	pb_acepta.Enabled = False
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para estos filtros.", StopSign!, Ok!)
	pb_acepta.Enabled = False
ELSE
	pb_acepta.Enabled = True
END IF



end event

type st_4 from statictext within w_proceso_temperatura_recepcion
integer x = 242
integer y = 448
integer width = 3090
integer height = 364
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

type em_recepcion from editmask within w_proceso_temperatura_recepcion
integer x = 928
integer y = 676
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF This.Text <> '' THEN
	IF ExisteRecepcion(Long(This.Text)) THEN
		This.Text = ''
		This.SetFocus()
	END IF	
END IF	
end event

type pb_1 from picturebutton within w_proceso_temperatura_recepcion
integer x = 3456
integer y = 1116
integer width = 302
integer height = 244
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;SetPointer(HourGlass!)

Long		fila, ll_Guia

str_info	lstr_info

IF em_recepcion.Text = 'none' OR em_recepcion.Text = '0' THEN
	MessageBox( "No Existe Recepción", "Falta EL Ingreso de Número Recepción.", StopSign!, Ok!)
	em_recepcion.SetFocus()
	Return
END IF	

ll_Guia	=	Long(em_recepcion.Text)

lstr_info.titulo	= "RECEPCION DE PALLETS TEMPERATURA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_recepcion_pallet_temperatura"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,ll_Guia)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("guia.text = '" + String(ll_Guia) + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
	
	
	
	
	
	

end event

type uo_selcliente from uo_seleccion_clientesprod within w_proceso_temperatura_recepcion
event destroy ( )
integer x = 923
integer y = 476
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_proceso_temperatura_recepcion
event destroy ( )
integer x = 923
integer y = 576
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

