$PBExportHeader$w_info_alertas_informes_produccion.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_info_alertas_informes_produccion from w_para_informes
end type
type gb_4 from groupbox within w_info_alertas_informes_produccion
end type
type st_3 from statictext within w_info_alertas_informes_produccion
end type
type st_2 from statictext within w_info_alertas_informes_produccion
end type
type dw_planta from datawindow within w_info_alertas_informes_produccion
end type
type st_6 from statictext within w_info_alertas_informes_produccion
end type
type dw_cliente from datawindow within w_info_alertas_informes_produccion
end type
type tab_1 from tab within w_info_alertas_informes_produccion
end type
type tabpage_1 from userobject within tab_1
end type
type dw_existencia from datawindow within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_existencia dw_existencia
end type
type tabpage_2 from userobject within tab_1
end type
type dw_existencia_prefrio from datawindow within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_existencia_prefrio dw_existencia_prefrio
end type
type tab_1 from tab within w_info_alertas_informes_produccion
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type
type uo_selespecie from uo_seleccion_especie within w_info_alertas_informes_produccion
end type
type st_4 from statictext within w_info_alertas_informes_produccion
end type
type rb_1 from radiobutton within w_info_alertas_informes_produccion
end type
type rb_2 from radiobutton within w_info_alertas_informes_produccion
end type
type cb_1 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_2 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_3 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_4 from commandbutton within w_info_alertas_informes_produccion
end type
type dw_1 from datawindow within w_info_alertas_informes_produccion
end type
type cb_5 from commandbutton within w_info_alertas_informes_produccion
end type
type pb_imprime from picturebutton within w_info_alertas_informes_produccion
end type
type rb_prefrio from radiobutton within w_info_alertas_informes_produccion
end type
type rb_existencia from radiobutton within w_info_alertas_informes_produccion
end type
type cb_6 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_7 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_8 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_9 from commandbutton within w_info_alertas_informes_produccion
end type
type cb_10 from commandbutton within w_info_alertas_informes_produccion
end type
type dw_2 from datawindow within w_info_alertas_informes_produccion
end type
type gb_3 from groupbox within w_info_alertas_informes_produccion
end type
type st_5 from statictext within w_info_alertas_informes_produccion
end type
type st_1 from statictext within w_info_alertas_informes_produccion
end type
type dw_grafico_prefrio from datawindow within w_info_alertas_informes_produccion
end type
type uo_selcamara from uo_seleccion_camarasbode_mod within w_info_alertas_informes_produccion
end type
type st_prefrio from statictext within w_info_alertas_informes_produccion
end type
type dw_grafico from datawindow within w_info_alertas_informes_produccion
end type
end forward

global type w_info_alertas_informes_produccion from w_para_informes
integer width = 3643
integer height = 2340
string title = "CONSULTA EN LINEA"
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
event ue_recuperadatos ( )
event ue_imprimir ( )
gb_4 gb_4
st_3 st_3
st_2 st_2
dw_planta dw_planta
st_6 st_6
dw_cliente dw_cliente
tab_1 tab_1
uo_selespecie uo_selespecie
st_4 st_4
rb_1 rb_1
rb_2 rb_2
cb_1 cb_1
cb_2 cb_2
cb_3 cb_3
cb_4 cb_4
dw_1 dw_1
cb_5 cb_5
pb_imprime pb_imprime
rb_prefrio rb_prefrio
rb_existencia rb_existencia
cb_6 cb_6
cb_7 cb_7
cb_8 cb_8
cb_9 cb_9
cb_10 cb_10
dw_2 dw_2
gb_3 gb_3
st_5 st_5
st_1 st_1
dw_grafico_prefrio dw_grafico_prefrio
uo_selcamara uo_selcamara
st_prefrio st_prefrio
dw_grafico dw_grafico
end type
global w_info_alertas_informes_produccion w_info_alertas_informes_produccion

type variables
datawindowchild  	idwc_planta, idwc_cliente

Integer				ii_CuantosLotes, ii_tipoinfo

str_busqueda		istr_busq
str_mant				istr_mant


uo_seleccion_especie		iuo_selespecie
uo_camarasbode				iuo_Camara
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean noexisteplanta (integer planta, integer cliente)
end prototypes

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dba.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean noexisteplanta (integer planta, integer cliente);Integer	codigo

	
	SELECT plde_codigo
	INTO	 :Codigo	
	FROM	dba.Plantadesp
	WHERE	plde_codigo	=	:planta;
//	  AND clie_codigo =  :cliente ;
	
	
	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas" )
			Return True			
	ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
			Return True						
	END IF

	Return False


end function

on w_info_alertas_informes_produccion.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_3=create st_3
this.st_2=create st_2
this.dw_planta=create dw_planta
this.st_6=create st_6
this.dw_cliente=create dw_cliente
this.tab_1=create tab_1
this.uo_selespecie=create uo_selespecie
this.st_4=create st_4
this.rb_1=create rb_1
this.rb_2=create rb_2
this.cb_1=create cb_1
this.cb_2=create cb_2
this.cb_3=create cb_3
this.cb_4=create cb_4
this.dw_1=create dw_1
this.cb_5=create cb_5
this.pb_imprime=create pb_imprime
this.rb_prefrio=create rb_prefrio
this.rb_existencia=create rb_existencia
this.cb_6=create cb_6
this.cb_7=create cb_7
this.cb_8=create cb_8
this.cb_9=create cb_9
this.cb_10=create cb_10
this.dw_2=create dw_2
this.gb_3=create gb_3
this.st_5=create st_5
this.st_1=create st_1
this.dw_grafico_prefrio=create dw_grafico_prefrio
this.uo_selcamara=create uo_selcamara
this.st_prefrio=create st_prefrio
this.dw_grafico=create dw_grafico
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_cliente
this.Control[iCurrent+7]=this.tab_1
this.Control[iCurrent+8]=this.uo_selespecie
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.rb_1
this.Control[iCurrent+11]=this.rb_2
this.Control[iCurrent+12]=this.cb_1
this.Control[iCurrent+13]=this.cb_2
this.Control[iCurrent+14]=this.cb_3
this.Control[iCurrent+15]=this.cb_4
this.Control[iCurrent+16]=this.dw_1
this.Control[iCurrent+17]=this.cb_5
this.Control[iCurrent+18]=this.pb_imprime
this.Control[iCurrent+19]=this.rb_prefrio
this.Control[iCurrent+20]=this.rb_existencia
this.Control[iCurrent+21]=this.cb_6
this.Control[iCurrent+22]=this.cb_7
this.Control[iCurrent+23]=this.cb_8
this.Control[iCurrent+24]=this.cb_9
this.Control[iCurrent+25]=this.cb_10
this.Control[iCurrent+26]=this.dw_2
this.Control[iCurrent+27]=this.gb_3
this.Control[iCurrent+28]=this.st_5
this.Control[iCurrent+29]=this.st_1
this.Control[iCurrent+30]=this.dw_grafico_prefrio
this.Control[iCurrent+31]=this.uo_selcamara
this.Control[iCurrent+32]=this.st_prefrio
this.Control[iCurrent+33]=this.dw_grafico
end on

on w_info_alertas_informes_produccion.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.dw_planta)
destroy(this.st_6)
destroy(this.dw_cliente)
destroy(this.tab_1)
destroy(this.uo_selespecie)
destroy(this.st_4)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.cb_1)
destroy(this.cb_2)
destroy(this.cb_3)
destroy(this.cb_4)
destroy(this.dw_1)
destroy(this.cb_5)
destroy(this.pb_imprime)
destroy(this.rb_prefrio)
destroy(this.rb_existencia)
destroy(this.cb_6)
destroy(this.cb_7)
destroy(this.cb_8)
destroy(this.cb_9)
destroy(this.cb_10)
destroy(this.dw_2)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.dw_grafico_prefrio)
destroy(this.uo_selcamara)
destroy(this.st_prefrio)
destroy(this.dw_grafico)
end on

event open;x	=	0
y	=	0

Boolean lb_Cerrar

tab_1.tabpage_1.dw_existencia.SetTransObject(sqlca)
tab_1.tabpage_2.dw_existencia_prefrio.SetTransObject(sqlca)

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SqlCa)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo",gi_codexport)
istr_busq.argum[2] = String(gi_codexport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
idwc_planta.Retrieve()
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gi_CodPlanta)
istr_busq.argum[1] = String(gi_CodPlanta)

dw_grafico.SetTransObject(Sqlca)
dw_grafico_prefrio.SetTransObject(Sqlca)
dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)

IF IsNull(uo_SelCamara.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCamara.Seleccion(True, False)
	uo_SelCamara.Filtra(gi_CodPlanta)
	uo_SelCamara.filtraprefrio()
	
	iuo_Camara	=	Create uo_camarasbode
END IF
// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

IF rb_existencia.Checked THEN
	cb_6.Visible = False
	cb_7.Visible = False
	cb_8.Visible = False
	cb_9.Visible = False
	cb_10.Visible = False
	dw_grafico_prefrio.Visible = False
	tab_1.tabpage_2.Enabled		= False
END IF	

end event

type st_computador from w_para_informes`st_computador within w_info_alertas_informes_produccion
end type

type st_usuario from w_para_informes`st_usuario within w_info_alertas_informes_produccion
end type

type st_temporada from w_para_informes`st_temporada within w_info_alertas_informes_produccion
end type

type p_logo from w_para_informes`p_logo within w_info_alertas_informes_produccion
end type

type st_titulo from w_para_informes`st_titulo within w_info_alertas_informes_produccion
integer x = 64
integer y = 32
integer width = 3214
integer height = 96
fontcharset fontcharset = ansi!
string text = "Alertas "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_alertas_informes_produccion
integer x = 3392
integer y = 256
integer taborder = 140
string picturename = "F:\Desarrollo\Bmp\BUSCAE.BMP"
end type

event pb_acepta::clicked;Long		ll_fila, respuesta, ll_tipo, ll_fila1, ll_fila2

IF rb_existencia.Checked THEN
	IF rb_1.Checked THEN
		ll_tipo = 2
		tab_1.tabpage_1.text ="Pallet Transitorios"
	ELSE
		ll_tipo = -1
		tab_1.tabpage_1.text ="Estadia de Pallet"
	END IF	
END IF	

cb_1.Enabled = False
cb_2.Enabled = False
cb_3.Enabled = False
cb_4.Enabled = False
cb_5.Enabled = False
cb_6.Enabled = False
cb_7.Enabled = False
cb_8.Enabled = False
cb_9.Enabled = False
cb_10.Enabled = False

dw_grafico.Reset()

IF rb_existencia.Checked THEN
	ll_fila = dw_grafico.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
											 uo_selespecie.Codigo,ll_tipo,Integer(-1))
ELSE											 
	ll_fila = dw_grafico_prefrio.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
											 uo_selespecie.Codigo,-1,Integer(-1),uo_SelCamara.Codigo)
END IF											 

IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
									Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF			

IF rb_existencia.Checked THEN
	ll_fila1 = dw_1.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
											 uo_selespecie.Codigo,ll_tipo,Integer(-1))
	FOR ll_fila2 = 1 TO dw_1.RowCount() 
		IF dw_1.Object.tipo[ll_fila2] = 1 AND cb_1.Enabled = False THEN
			cb_1.Enabled = True
			cb_5.Enabled = True
		ELSEIF dw_1.Object.tipo[ll_fila2] = 2 AND cb_2.Enabled = False	THEN
			cb_2.Enabled = True
			cb_5.Enabled = True
		ELSEIF dw_1.Object.tipo[ll_fila2] = 3 AND cb_3.Enabled = False	THEN	
			cb_3.Enabled = True
			cb_5.Enabled = True
		ELSEIF dw_1.Object.tipo[ll_fila2] = 4 AND cb_4.Enabled = False	THEN		
			cb_4.Enabled = True
			cb_5.Enabled = True
		END IF	
	
	NEXT										 
ELSE		
	ll_fila1 = dw_2.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
											 uo_selespecie.Codigo,-1,Integer(-1),uo_SelCamara.Codigo)
	FOR ll_fila2 = 1 TO dw_2.RowCount() 
		IF dw_2.Object.tipo[ll_fila2] = 1 AND cb_6.Enabled = False THEN
			cb_6.Enabled = True
			cb_10.Enabled = True
		ELSEIF dw_2.Object.tipo[ll_fila2] = 2 AND cb_7.Enabled = False	THEN
			cb_7.Enabled = True
			cb_10.Enabled = True
		ELSEIF dw_2.Object.tipo[ll_fila2] = 3 AND cb_8.Enabled = False	THEN	
			cb_8.Enabled = True
			cb_10.Enabled = True
		ELSEIF dw_2.Object.tipo[ll_fila2] = 4 AND cb_9.Enabled = False	THEN		
			cb_9.Enabled = True
			cb_10.Enabled = True
		END IF	
		
	NEXT
	ll_fila1 = dw_2.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
											 uo_selespecie.Codigo,-1,Integer(-1),uo_SelCamara.Codigo)
END IF											 

tab_1.tabpage_1.dw_existencia.Reset()
tab_1.tabpage_2.dw_existencia_prefrio.Reset()

end event

type pb_salir from w_para_informes`pb_salir within w_info_alertas_informes_produccion
integer x = 3392
integer y = 1008
integer taborder = 190
end type

type gb_4 from groupbox within w_info_alertas_informes_produccion
integer x = 2656
integer y = 144
integer width = 585
integer height = 328
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_3 from statictext within w_info_alertas_informes_produccion
integer x = 96
integer y = 288
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_alertas_informes_produccion
integer x = 59
integer y = 136
integer width = 2555
integer height = 372
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_alertas_informes_produccion
integer x = 306
integer y = 280
integer width = 1175
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long  ll_Null

IF NoExistePlanta(Integer(Data),gi_codexport) THEN
	This.SetItem(Row, "plde_codigo",IsNull(ll_Null))
	Return 1
ELSE
	istr_busq.Argum[1]	= Data
	//em_recepcion.SetFocus()
//	em_recepcion.text=''
//	dw_lotes.Reset()

END IF
	

end event

event itemerror;Return 1
end event

type st_6 from statictext within w_info_alertas_informes_produccion
integer x = 96
integer y = 176
integer width = 229
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_alertas_informes_produccion
integer x = 306
integer y = 168
integer width = 1157
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long  ll_Null

IF NoExisteCliente(Integer(Data)) THEN
	This.SetItem(Row, "clie_codigo",IsNull(ll_Null))
	Return 1
ELSE
	istr_busq.Argum[2]	= Data
	dw_planta.SetFocus()
END IF
	
end event

event itemerror;RETURN 1
end event

type tab_1 from tab within w_info_alertas_informes_produccion
integer x = 37
integer y = 1280
integer width = 3310
integer height = 964
integer taborder = 210
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.Control[]={this.tabpage_1,&
this.tabpage_2}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3273
integer height = 836
long backcolor = 12632256
string text = "Existencia"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_existencia dw_existencia
end type

on tabpage_1.create
this.dw_existencia=create dw_existencia
this.Control[]={this.dw_existencia}
end on

on tabpage_1.destroy
destroy(this.dw_existencia)
end on

type dw_existencia from datawindow within tabpage_1
integer x = 5
integer y = 16
integer width = 3264
integer height = 792
integer taborder = 30
string title = "none"
string dataobject = "dw_info_mues_consulta_existencia"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3273
integer height = 836
long backcolor = 12632256
string text = "Prefrío"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_existencia_prefrio dw_existencia_prefrio
end type

on tabpage_2.create
this.dw_existencia_prefrio=create dw_existencia_prefrio
this.Control[]={this.dw_existencia_prefrio}
end on

on tabpage_2.destroy
destroy(this.dw_existencia_prefrio)
end on

type dw_existencia_prefrio from datawindow within tabpage_2
integer y = 16
integer width = 3273
integer height = 816
integer taborder = 30
string title = "none"
string dataobject = "dw_info_mues_consulta_prefrio"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
end type

type uo_selespecie from uo_seleccion_especie within w_info_alertas_informes_produccion
integer x = 1714
integer y = 152
integer height = 192
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_4 from statictext within w_info_alertas_informes_produccion
integer x = 1490
integer y = 220
integer width = 229
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_alertas_informes_produccion
integer x = 357
integer y = 404
integer width = 645
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Pallet Transitorios"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_alertas_informes_produccion
integer x = 1687
integer y = 404
integer width = 608
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Estadia de Pallet  "
end type

type cb_1 from commandbutton within w_info_alertas_informes_produccion
integer x = 2683
integer y = 540
integer width = 402
integer height = 112
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "1 a 10"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_1.dw_existencia.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,ll_tipo,1)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF	

ii_tipoinfo = 1

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type cb_2 from commandbutton within w_info_alertas_informes_produccion
integer x = 2683
integer y = 700
integer width = 402
integer height = 112
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "11 a 20"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_1.dw_existencia.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,ll_tipo,2)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF

ii_tipoinfo = 2

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF	
end event

type cb_3 from commandbutton within w_info_alertas_informes_produccion
integer x = 2683
integer y = 860
integer width = 402
integer height = 112
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "21 a 30"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_1.dw_existencia.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,ll_tipo,3)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF	

ii_tipoinfo = 3

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type cb_4 from commandbutton within w_info_alertas_informes_produccion
integer x = 2683
integer y = 1020
integer width = 402
integer height = 112
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "31 o Más"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_1.dw_existencia.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,ll_tipo,4)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF

ii_tipoinfo = 4

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type dw_1 from datawindow within w_info_alertas_informes_produccion
boolean visible = false
integer x = 489
integer y = 2252
integer width = 686
integer height = 400
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_mues_consulta_existencia"
boolean border = false
boolean livescroll = true
end type

type cb_5 from commandbutton within w_info_alertas_informes_produccion
integer x = 2683
integer y = 1180
integer width = 402
integer height = 112
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Todos"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_1.dw_existencia.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,ll_tipo,-1)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF	

ii_tipoinfo = -1

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type pb_imprime from picturebutton within w_info_alertas_informes_produccion
integer x = 3392
integer y = 632
integer width = 233
integer height = 196
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\ImprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\ImprimirDisab.png"
alignment htextalign = left!
end type

event clicked;SetPointer(HourGlass!)

Long		ll_fila, respuesta, ll_tipo, ll_fila1, ll_fila2, fila

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

istr_info.titulo	= "ALERTA"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

IF rb_existencia.Checked THEN
	vinf.dw_1.DataObject = "dw_info_consulta_existencia"
	vinf.dw_1.SetTransObject(sqlca)
	fila = vinf.dw_1.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
										 uo_selespecie.Codigo,ll_tipo,Integer(ii_tipoinfo))
ELSE
	vinf.dw_1.DataObject = "dw_info_consulta_prefrio"
	ll_tipo = -1
	vinf.dw_1.SetTransObject(sqlca)
	fila = vinf.dw_1.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
										 uo_selespecie.Codigo,ll_tipo,Integer(ii_tipoinfo),uo_SelCamara.Codigo)
END IF	

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	IF rb_existencia.Checked THEN
		vinf.dw_1.Modify("t_20.text = '" + tab_1.tabpage_1.text + "'")
	ELSE
		vinf.dw_1.Modify("t_20.text = '" + tab_1.tabpage_2.text + "'")
	END IF	
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type rb_prefrio from radiobutton within w_info_alertas_informes_produccion
integer x = 2747
integer y = 332
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Prefrío"
end type

event clicked;cb_1.Visible = False
cb_2.Visible = False
cb_3.Visible = False
cb_4.Visible = False
cb_5.Visible = False
cb_6.Visible = True
cb_7.Visible = True
cb_8.Visible = True
cb_9.Visible = True
cb_10.Visible = True
//cb_6.Enabled = True
//cb_7.Enabled = True
//cb_8.Enabled = True
//cb_9.Enabled = True
//cb_10.Enabled = True

cb_1.Enabled = False
cb_2.Enabled = False
cb_3.Enabled = False
cb_4.Enabled = False
cb_5.Enabled = False
cb_6.Enabled = False
cb_7.Enabled = False
cb_8.Enabled = False
cb_9.Enabled = False
cb_10.Enabled = False

dw_grafico_prefrio.Visible = True
dw_grafico.Visible = False
tab_1.tabpage_2.Enabled		= True
tab_1.tabpage_1.Enabled		= False

uo_selcamara.Visible = True
st_prefrio.Visible = True

tab_1.SelectedTab = 2

rb_1.Visible = False
rb_2.Visible = False
end event

type rb_existencia from radiobutton within w_info_alertas_informes_produccion
integer x = 2752
integer y = 224
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Existencia"
boolean checked = true
end type

event clicked;cb_1.Visible = True
cb_2.Visible = True
cb_3.Visible = True
cb_4.Visible = True
cb_5.Visible = True
cb_6.Visible = False
cb_7.Visible = False
cb_8.Visible = False
cb_9.Visible = False
cb_10.Visible = False

cb_1.Enabled = False
cb_2.Enabled = False
cb_3.Enabled = False
cb_4.Enabled = False
cb_5.Enabled = False
cb_6.Enabled = False
cb_7.Enabled = False
cb_8.Enabled = False
cb_9.Enabled = False
cb_10.Enabled = False

dw_grafico_prefrio.Visible = False
dw_grafico.Visible = True
tab_1.tabpage_2.Enabled		= False
tab_1.tabpage_1.Enabled		= True
rb_1.Visible = True
rb_2.Visible = True
tab_1.SelectedTab = 1

uo_selcamara.Visible 	= False
st_prefrio.Visible	 	= False
end event

type cb_6 from commandbutton within w_info_alertas_informes_produccion
boolean visible = false
integer x = 2683
integer y = 540
integer width = 402
integer height = 112
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "0 a 2"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_2.dw_existencia_prefrio.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,-1,1,uo_SelCamara.Codigo)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF	

ii_tipoinfo = 1

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type cb_7 from commandbutton within w_info_alertas_informes_produccion
boolean visible = false
integer x = 2683
integer y = 700
integer width = 402
integer height = 112
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "3 a 4"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_2.dw_existencia_prefrio.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,-1,2,uo_SelCamara.Codigo)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF

ii_tipoinfo = 2

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF	
end event

type cb_8 from commandbutton within w_info_alertas_informes_produccion
boolean visible = false
integer x = 2683
integer y = 860
integer width = 402
integer height = 112
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "5 a 8"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_2.dw_existencia_prefrio.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,-1,3,uo_SelCamara.Codigo)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF	

ii_tipoinfo = 3

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type cb_9 from commandbutton within w_info_alertas_informes_produccion
boolean visible = false
integer x = 2683
integer y = 1020
integer width = 402
integer height = 112
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "9 o Más"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_2.dw_existencia_prefrio.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,-1,4,uo_SelCamara.Codigo)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF

ii_tipoinfo = 4

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type cb_10 from commandbutton within w_info_alertas_informes_produccion
boolean visible = false
integer x = 2683
integer y = 1172
integer width = 402
integer height = 112
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Todos"
end type

event clicked;Long	ll_fila, ll_tipo, respuesta

IF rb_1.Checked THEN
	ll_tipo = 2
ELSE
	ll_tipo = -1
END IF	

ll_fila = tab_1.tabpage_2.dw_existencia_prefrio.Retrieve(Integer(istr_busq.Argum[2]),Integer(istr_busq.Argum[1]),&
				uo_selespecie.Codigo,-1,-1,uo_SelCamara.Codigo)
				
IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF	

ii_tipoinfo = -1

IF ll_fila > 0 THEN
	pb_imprime.Enabled = True
END IF
end event

type dw_2 from datawindow within w_info_alertas_informes_produccion
boolean visible = false
integer x = 3433
integer y = 2020
integer width = 686
integer height = 400
integer taborder = 220
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_mues_consulta_prefrio"
boolean border = false
boolean livescroll = true
end type

type gb_3 from groupbox within w_info_alertas_informes_produccion
boolean visible = false
integer x = 3328
integer y = 552
integer width = 274
integer height = 268
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
end type

type st_5 from statictext within w_info_alertas_informes_produccion
integer x = 2615
integer y = 136
integer width = 663
integer height = 372
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_alertas_informes_produccion
integer x = 37
integer y = 8
integer width = 3269
integer height = 520
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_grafico_prefrio from datawindow within w_info_alertas_informes_produccion
boolean visible = false
integer x = 937
integer y = 532
integer width = 1655
integer height = 784
integer taborder = 110
string title = "none"
string dataobject = "dw_grafico_consulta_prefrio"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type uo_selcamara from uo_seleccion_camarasbode_mod within w_info_alertas_informes_produccion
boolean visible = false
integer x = 311
integer y = 372
integer taborder = 70
boolean bringtotop = true
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode_mod::destroy
end on

type st_prefrio from statictext within w_info_alertas_informes_produccion
boolean visible = false
integer x = 96
integer y = 400
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Prefrio"
boolean focusrectangle = false
end type

type dw_grafico from datawindow within w_info_alertas_informes_produccion
integer x = 937
integer y = 532
integer width = 1655
integer height = 784
integer taborder = 40
string title = "none"
string dataobject = "dw_grafico_consulta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

