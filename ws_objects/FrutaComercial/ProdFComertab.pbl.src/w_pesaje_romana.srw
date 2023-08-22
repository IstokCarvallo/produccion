$PBExportHeader$w_pesaje_romana.srw
forward
global type w_pesaje_romana from window
end type
type ole_puerta from olecustomcontrol within w_pesaje_romana
end type
type em_bultos from editmask within w_pesaje_romana
end type
type st_5 from statictext within w_pesaje_romana
end type
type ddlb_tipo from dropdownlistbox within w_pesaje_romana
end type
type st_6 from statictext within w_pesaje_romana
end type
type em_nropesa from editmask within w_pesaje_romana
end type
type st_1 from statictext within w_pesaje_romana
end type
type st_2 from statictext within w_pesaje_romana
end type
type st_3 from statictext within w_pesaje_romana
end type
type st_4 from statictext within w_pesaje_romana
end type
type ddlb_estado from dropdownlistbox within w_pesaje_romana
end type
type em_pesref from editmask within w_pesaje_romana
end type
type em_difpor from editmask within w_pesaje_romana
end type
type pb_limpia from picturebutton within w_pesaje_romana
end type
type sle_cadini from singlelineedit within w_pesaje_romana
end type
type sle_lectura from singlelineedit within w_pesaje_romana
end type
type cb_1 from commandbutton within w_pesaje_romana
end type
type cb_setup from commandbutton within w_pesaje_romana
end type
type em_kilos from editmask within w_pesaje_romana
end type
type st_fondo from statictext within w_pesaje_romana
end type
type pb_salir from picturebutton within w_pesaje_romana
end type
type pb_elimina from picturebutton within w_pesaje_romana
end type
type pb_inserta from picturebutton within w_pesaje_romana
end type
type dw_1 from datawindow within w_pesaje_romana
end type
type gb_1 from groupbox within w_pesaje_romana
end type
type gb_2 from groupbox within w_pesaje_romana
end type
type gb_3 from groupbox within w_pesaje_romana
end type
type gb_4 from groupbox within w_pesaje_romana
end type
type gb_5 from groupbox within w_pesaje_romana
end type
type str_pesaje from structure within w_pesaje_romana
end type
end forward

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

global type w_pesaje_romana from window
integer width = 2633
integer height = 2508
windowtype windowtype = response!
long backcolor = 12632256
ole_puerta ole_puerta
em_bultos em_bultos
st_5 st_5
ddlb_tipo ddlb_tipo
st_6 st_6
em_nropesa em_nropesa
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
ddlb_estado ddlb_estado
em_pesref em_pesref
em_difpor em_difpor
pb_limpia pb_limpia
sle_cadini sle_cadini
sle_lectura sle_lectura
cb_1 cb_1
cb_setup cb_setup
em_kilos em_kilos
st_fondo st_fondo
pb_salir pb_salir
pb_elimina pb_elimina
pb_inserta pb_inserta
dw_1 dw_1
gb_1 gb_1
gb_2 gb_2
gb_3 gb_3
gb_4 gb_4
gb_5 gb_5
end type
global w_pesaje_romana w_pesaje_romana

type variables
Long					il_fila, il_medioseg, ii_tipo = 1, il_autoincrement
Double				id_kilos
Integer				ii_pesajenuevo, ii_Estado, ii_PesajeAnt, il_bultos
Boolean				 ib_OCX 

str_mant 			istr_mant, istr_mant2

uo_bins				iuo_bins 
Private:
str_pesaje			wstr_pesaje
str_puertacomm	istr_puertacomm
end variables

forward prototypes
public subroutine filtra (string as_valor)
public subroutine habilitaencab (boolean habilita)
public function decimal calculo (decimal ad_total, decimal ad_valref)
public function boolean existepesaje (integer ai_nropesaje)
public function boolean existetarja (long ai_tarja, integer ai_cliente, integer ai_planta)
public function boolean existeromana (integer ai_planta)
end prototypes

public subroutine filtra (string as_valor);dw_1.SetFilter("")
dw_1.Filter()

dw_1.SetFilter("mfgp_nropes = " + as_Valor)
dw_1.Filter()
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	em_nropesa.Enabled	=	True
	pb_inserta.Enabled	=	False
	pb_elimina.Enabled	=	False
ELSE
	//em_nropesa.Enabled	=	False
	pb_inserta.Enabled	=	True
	pb_elimina.Enabled	=	True
END IF
end subroutine

public function decimal calculo (decimal ad_total, decimal ad_valref);Decimal{2}	ld_resultado

IF IsNull(ad_Total) OR ad_Total = 0 THEN ad_Total = 1

ld_resultado	=	Abs(ad_valref - ad_total) * 100 / ad_total

Return ld_resultado
end function

public function boolean existepesaje (integer ai_nropesaje);Long		ll_Fila

ll_Fila	=	dw_1.Find("mfgp_nropes = " + String(ai_nropesaje) , 1, dw_1.RowCount())

IF ll_Fila > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF

		
end function

public function boolean existetarja (long ai_tarja, integer ai_cliente, integer ai_planta);Boolean 	lb_retorno
Integer	li_count

lb_Retorno	=	TRUE

SELECT Count(*)
INTO :li_count
FROM dba.spro_movtobins
WHERE clie_codigo =: ai_cliente
	 and plde_codigo =: ai_planta
	 and fgmb_nrotar =: ai_tarja;
	 
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
	lb_Retorno	=	TRUE
	
ELSEIF IsNull(li_count)THEN
	lb_Retorno	=	FALSE
	
ELSEIF  li_count = 0 THEN
	lb_Retorno	=	FALSE
	
ELSE
	MessageBox("Atención", "La Tarja " + String(ai_tarja) + &
 					", ya fue ingresada.~r~rIngrese o seleccione otra Tarja.")
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public function boolean existeromana (integer ai_planta);Boolean 	lb_retorno
Integer	li_count

lb_Retorno	=	TRUE

SELECT IsNull(Count(*),0)
INTO :li_count
FROM  dba.plantaconfromana
WHERE  plde_codigo =: ai_planta
AND crpl_equcon =: gstr_us.computador;	 
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla plantaconfromana")
	lb_Retorno	=	TRUE	
ELSEIF  li_count = 0 THEN
	MessageBox("Atención", "Computador no Tiene Asignado Romana")
	lb_Retorno	=	TRUE
ELSE 
	lb_Retorno	=	FALSE
END IF

RETURN lb_Retorno
end function

on w_pesaje_romana.create
this.ole_puerta=create ole_puerta
this.em_bultos=create em_bultos
this.st_5=create st_5
this.ddlb_tipo=create ddlb_tipo
this.st_6=create st_6
this.em_nropesa=create em_nropesa
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.ddlb_estado=create ddlb_estado
this.em_pesref=create em_pesref
this.em_difpor=create em_difpor
this.pb_limpia=create pb_limpia
this.sle_cadini=create sle_cadini
this.sle_lectura=create sle_lectura
this.cb_1=create cb_1
this.cb_setup=create cb_setup
this.em_kilos=create em_kilos
this.st_fondo=create st_fondo
this.pb_salir=create pb_salir
this.pb_elimina=create pb_elimina
this.pb_inserta=create pb_inserta
this.dw_1=create dw_1
this.gb_1=create gb_1
this.gb_2=create gb_2
this.gb_3=create gb_3
this.gb_4=create gb_4
this.gb_5=create gb_5
this.Control[]={this.ole_puerta,&
this.em_bultos,&
this.st_5,&
this.ddlb_tipo,&
this.st_6,&
this.em_nropesa,&
this.st_1,&
this.st_2,&
this.st_3,&
this.st_4,&
this.ddlb_estado,&
this.em_pesref,&
this.em_difpor,&
this.pb_limpia,&
this.sle_cadini,&
this.sle_lectura,&
this.cb_1,&
this.cb_setup,&
this.em_kilos,&
this.st_fondo,&
this.pb_salir,&
this.pb_elimina,&
this.pb_inserta,&
this.dw_1,&
this.gb_1,&
this.gb_2,&
this.gb_3,&
this.gb_4,&
this.gb_5}
end on

on w_pesaje_romana.destroy
destroy(this.ole_puerta)
destroy(this.em_bultos)
destroy(this.st_5)
destroy(this.ddlb_tipo)
destroy(this.st_6)
destroy(this.em_nropesa)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.ddlb_estado)
destroy(this.em_pesref)
destroy(this.em_difpor)
destroy(this.pb_limpia)
destroy(this.sle_cadini)
destroy(this.sle_lectura)
destroy(this.cb_1)
destroy(this.cb_setup)
destroy(this.em_kilos)
destroy(this.st_fondo)
destroy(this.pb_salir)
destroy(this.pb_elimina)
destroy(this.pb_inserta)
destroy(this.dw_1)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.gb_5)
end on

event open;Integer	li_Resultado
Long		ll_Elemento, ll_Fila
String		ls_Parametros


wstr_pesaje	=	Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
wstr_pesaje.dw.ShareData(dw_1)

il_autoincrement = 0
FOR ll_fila = 1 to dw_1.RowCount()
	IF il_autoincrement < dw_1.Object.mfgp_nropes[ll_fila] THEN
		il_autoincrement = dw_1.Object.mfgp_nropes[ll_fila]
	END IF
NEXT
il_autoincrement = il_autoincrement + 1

dw_1.SetTransObject(SQLCA)

IF dw_1.RowCount() > 0 THEN
	Filtra("-1")
END IF

istr_puertacomm	=	wstr_pesaje.puerta

IF istr_puertacomm.pesajebins = 1 AND NOT ib_OCX THEN
	pb_inserta.PictureName = '\desarrollo\bmp\inserte.bmp'
	pb_inserta.DisabledName = '\desarrollo\bmp\insertd.bmp'
	pb_elimina.PictureName = '\desarrollo\bmp\suprime.bmp'
	pb_elimina.DisabledName = '\desarrollo\bmp\suprimd.bmp'
ELSE
	pb_inserta.PictureName = '\desarrollo\bmp\aceptae.bmp'
	pb_inserta.DisabledName = '\desarrollo\bmp\aceptad.bmp'
	pb_elimina.PictureName = '\desarrollo\bmp\cancelae.bmp'
	pb_elimina.DisabledName = '\desarrollo\bmp\cancelad.bmp'
END IF

ddlb_estado.SelectItem (1)
ddlb_Tipo.SelectItem (2)

ii_Estado	=	2

iuo_bins		=	Create uo_bins

em_nropesa.Text = String(il_autoincrement)

li_resultado 	=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado = 0 THEN
	ls_parametros	=	String(istr_puertacomm.Baudios)+","+&
							istr_puertacomm.Paridad+","+&
							String(istr_puertacomm.Data)+","+&
							String(istr_puertacomm.Parada)
			
	IF ExisteRomana(Integer(wstr_pesaje.argum[1])) THEN
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
		ib_OCX = False
	ELSE
		ib_OCX =	True
		//li_resultado 	= Ole_puerta.object.Open(istr_puertacomm.Puerta, ls_parametros)
		IF Ole_puerta.object.PortOpen THEN Ole_puerta.object.PortOpen = False
		Ole_puerta.object.settings = ls_parametros
		Ole_puerta.object.PortOpen = True
	END IF
END IF

em_nropesa.TriggerEvent("Modified")
end event

event timer;Integer	li_factor, li_posini, li_LarBuf
String 	ls_string
Double	ld_kilos

//ls_string = w_maed_movtofrutagranel_recepcion.Ole_Puerta.Object.ReadString(istr_puertacomm.LargoLectura) 
 Ole_Puerta.Object.inputlen =	Integer(istr_puertacomm.LargoLectura)

li_LarBuf =	Ole_Puerta.Object.InBufferCount

IF li_LarBuf > 0 THEN
	ls_string =  Ole_Puerta.Object.input
END IF

li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)

IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
	
IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
	ld_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
	IF istr_puertacomm.Decimales > 0 THEN
		li_factor	= 10 ^ istr_puertacomm.Decimales
		ld_kilos		= Round(ld_kilos/li_factor,istr_puertacomm.Decimales)
	END IF
	em_kilos.Text	=	String(ld_kilos)
END IF

IF ld_kilos < istr_puertacomm.PesoMinimo THEN
	ii_pesajenuevo	=	0
	RETURN
END IF

IF ii_pesajenuevo = 1 THEN 
	RETURN
END IF

IF ld_kilos = id_kilos THEN
	il_medioseg ++
ELSE
	id_kilos		=	ld_kilos
	il_medioseg =	1
END IF

IF il_medioseg / 2 >= istr_puertacomm.Estabilidad THEN
	pb_elimina.Enabled	=	True
	
	il_Fila	=	dw_1.InsertRow(0)
	
	dw_1.Object.mfgp_horaev[il_fila]	=	DateTime(Today(),Now())
	dw_1.Object.mfgp_pesore[il_fila]	=	ld_kilos
	dw_1.SetRow(il_Fila)
	
//	dw_1.SetColumn("mfgp_pesore")
	dw_1.SetColumn("fgmb_nrotar")
//	dw_1.SetColumn(10)
	ii_pesajenuevo	=	1
	il_medioseg		=	1
END IF
end event

event close;//IF ib_OCX THEN
//	ole_Puerta.Object.Close()
//END IF
//
end event

type ole_puerta from olecustomcontrol within w_pesaje_romana
event oncomm ( )
integer x = 2345
integer y = 1608
integer width = 174
integer height = 152
integer taborder = 130
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_pesaje_romana.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type em_bultos from editmask within w_pesaje_romana
integer x = 603
integer y = 512
integer width = 338
integer height = 92
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "0"
end type

event losefocus;Integer  	li_filas
Decimal	ldec_kilos

pb_elimina.Enabled	=	True

il_bultos 				= 	Integer(em_bultos.Text)
ldec_kilos				=	Dec(em_kilos.Text)
em_pesref.Text			=	em_kilos.Text

IF IsNull(il_bultos) OR il_bultos < 1 THEN 
	MessageBox("Error", "Debe registrar la cantidad de bultos antes de ingresar las tarjas", Exclamation!)
	RETURN 1
END IF
IF IsNull(ldec_kilos) OR ldec_kilos < 1 THEN 
	MessageBox("Error", "Debe registrar los kilos de la romana antes de ingresar las tarjas", Exclamation!)
	RETURN 1
END IF

em_bultos.Text			= 	String(il_bultos)
em_bultos.Enabled 	= 	False

IF istr_puertacomm.pesajebins = 1 AND NOT ib_OCX THEN
	FOR li_filas = 1 to il_bultos
		il_Fila	=	dw_1.InsertRow(0)
	
		dw_1.Object.mfgp_nropes[il_fila]			=	Integer(em_nropesa.Text)
		dw_1.Object.mfgp_valref[il_Fila]			=	Dec(em_pesref.Text)
		dw_1.Object.mfgp_estado[il_fila]			=	ii_Estado
		dw_1.Object.mfgp_horaev[il_fila]			=	Time(Today())	
		dw_1.Object.mfgp_tippes[il_fila]			=	ii_tipo
		dw_1.Object.mfgp_secuen[il_fila]			=	il_fila
		dw_1.SetRow(il_Fila)
		dw_1.Object.mfgp_estado[il_fila] 			= 	2
		dw_1.Object.Lote_codigo[il_fila] 		= 	Integer(wstr_pesaje.Argum[7])
		dw_1.Object.mfgp_pesore[il_fila]		=	Dec(em_kilos.Text) / il_bultos		
		dw_1.GroupCalc()
		em_difpor.Text 							= String(Calculo(dw_1.Object.total[1], Dec(em_pesref.Text)))
		ii_pesajenuevo								=	1
		il_medioseg									=	1
		NEXT	
		
		dw_1.SetRow(il_Fila - il_bultos)
		dw_1.SetColumn(10)
ELSE
	IF Dec(em_kilos.Text) >= istr_puertacomm.PesoMinimo THEN
		
		FOR li_filas = 1 to il_bultos
			
			il_Fila											=	dw_1.InsertRow(0)
			
			dw_1.Object.mfgp_nropes[il_fila]		=	Integer(em_nropesa.Text)
			dw_1.Object.mfgp_valref[il_Fila]		=	Dec(em_pesref.Text)
			dw_1.Object.mfgp_estado[il_fila]		=	ii_Estado
			dw_1.Object.mfgp_horaev[il_fila]		=	Time(Now())
			dw_1.Object.mfgp_tippes[il_fila]		=	ii_tipo
			dw_1.Object.mfgp_secuen[il_fila]		=	il_fila	
			dw_1.Object.mfgp_estado[il_fila] 		= 	2
			dw_1.Object.Lote_codigo[il_fila] 		= 	Integer(wstr_pesaje.Argum[7])
			dw_1.Object.mfgp_pesore[il_fila]		=	Dec(em_kilos.Text) / il_bultos
			dw_1.GroupCalc()
			em_difpor.Text 							= String(Calculo(dw_1.Object.total[1], Dec(em_pesref.Text)))
			ii_pesajenuevo								=	1
			il_medioseg									=	1
		NEXT	
		
		dw_1.SetRow(il_Fila - il_bultos)
		dw_1.SetColumn(10)
		
	END IF
END IF
end event

type st_5 from statictext within w_pesaje_romana
integer x = 82
integer y = 524
integer width = 503
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Cant. Bultos"
boolean focusrectangle = false
end type

type ddlb_tipo from dropdownlistbox within w_pesaje_romana
integer x = 1303
integer y = 512
integer width = 718
integer height = 400
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
boolean sorted = false
string item[] = {"Real","Referencia"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tipo = Index - 1
end event

type st_6 from statictext within w_pesaje_romana
integer x = 987
integer y = 524
integer width = 320
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Tipo Peso"
boolean focusrectangle = false
end type

type em_nropesa from editmask within w_pesaje_romana
integer x = 603
integer y = 296
integer width = 283
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "##"
end type

event modified;Decimal	ld_total
Long		ll_Fila
Integer  li_Tipo

dw_1.SetRedraw(False)

dw_1.SetFilter("")
dw_1.Filter()

dw_1.SetFilter("mfgp_estado = 2")
dw_1.Filter()

IF dw_1.RowCount() > 0 THEN
	ii_PesajeAnt	=	dw_1.Object.mfgp_nropes[1]
ELSE
	ii_PesajeAnt	=	0
END IF
dw_1.SetFilter("")
dw_1.Filter()

IF Existepesaje(Integer(This.Text)) THEN
	Filtra(This.Text)
	
	IF dw_1.RowCount() > 0 THEN
		em_pesref.Text	=	String(dw_1.Object.mfgp_valref[1])
		ii_Estado				=	ddlb_estado.SelectItem(dw_1.Object.mfgp_estado[1])
		ld_total				=	dw_1.Object.total[1]
		 em_bultos.Text	=	String(dw_1.RowCount())
		dw_1.GroupCalc()
		em_difpor.Text		=	String(Calculo(ld_total, dw_1.Object.mfgp_valref[1]))
		li_Tipo				=	ddlb_tipo.SelectItem(dw_1.Object.mfgp_tippes[1]+1)
	END IF
ELSE
	This.Text	=	String(il_autoincrement) 
	FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.mfgp_estado[ll_Fila] = 2
	NEXT
	
	ii_Estado		=	ddlb_estado.SelectItem(2)	
	Filtra("-1")
END IF

HabilitaEncab(False)

IF ib_OCX THEN
	Timer(0.2)
END IF

dw_1.SetRedraw(True)
end event

type st_1 from statictext within w_pesaje_romana
integer x = 82
integer y = 308
integer width = 453
integer height = 68
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Nro. de Pesaje"
boolean focusrectangle = false
end type

type st_2 from statictext within w_pesaje_romana
integer x = 82
integer y = 412
integer width = 489
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Peso Referencia"
boolean focusrectangle = false
end type

type st_3 from statictext within w_pesaje_romana
integer x = 987
integer y = 312
integer width = 283
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Estado"
boolean focusrectangle = false
end type

type st_4 from statictext within w_pesaje_romana
integer x = 987
integer y = 416
integer width = 283
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Dif.  %"
boolean focusrectangle = false
end type

type ddlb_estado from dropdownlistbox within w_pesaje_romana
integer x = 1303
integer y = 300
integer width = 585
integer height = 300
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
boolean sorted = false
string item[] = {"No Oficial","Oficial"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Long	ll_Fila

ii_Estado	=	index

IF ii_Estado = 2 THEN
	FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.mfgp_estado[ll_Fila] = 2
	NEXT
	
	FOR ll_Fila = 1 TO dw_1.FilteredCount()
		dw_1.Object.mfgp_estado.Filter[ll_Fila] = 1
	NEXT
ELSE
	IF em_nropesa.Text = String(ii_PesajeAnt) OR ii_PesajeAnt = 0 THEN
		Messagebox("Atención","No hay otro pesaje oficial, no se puede modificar")
		This.SelectItem ( 2 )
	ELSE		
		FOR ll_Fila = 1 TO dw_1.RowCount()
			dw_1.Object.mfgp_estado[ll_Fila] = 1
		NEXT
		
		FOR ll_Fila = 1 TO dw_1.FilteredCount()
			IF dw_1.Object.mfgp_nropes.Filter[ll_Fila] = ii_PesajeAnt THEN
				dw_1.Object.mfgp_estado.Filter[ll_Fila] = 2
			ELSE
				dw_1.Object.mfgp_estado.Filter[ll_Fila] = 1
			END IF
		NEXT
	END IF
	
END IF
end event

type em_pesref from editmask within w_pesaje_romana
integer x = 603
integer y = 400
integer width = 338
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "###,###.0000"
end type

event modified;Long	ll_Fila

IF dw_1.RowCount() > 0 THEN
	
	dw_1.GroupCalc()
				
	em_difpor.Text = String(Calculo(dw_1.Object.total[1], Dec(This.Text)))
	
END IF

FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.mfgp_valref[ll_Fila] = Dec(This.Text)
	NEXT
	
FOR ll_Fila = 1 TO dw_1.FilteredCount()
	dw_1.Object.mfgp_valref.Filter[ll_Fila] = Dec(This.Text)
NEXT
end event

event losefocus;//If Dec(This.Text) < 1 AND NOT IsNull(This.Text) Then
//	MessageBox('Error', 'Debe ingresar un valor para el peso de referencia.')
//	This.SetFocus()
//	Return 1
//End If
end event

type em_difpor from editmask within w_pesaje_romana
integer x = 1303
integer y = 400
integer width = 283
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
end type

type pb_limpia from picturebutton within w_pesaje_romana
boolean visible = false
integer x = 2327
integer y = 484
integer width = 155
integer height = 132
integer taborder = 110
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Nuevoe.bmp"
string disabledname = "\Desarrollo\Bmp\Nuevod.bmp"
alignment htextalign = left!
end type

event clicked;IF dw_1.RowCount() > 0 AND (em_difpor.Text = "" OR &
	IsNull(em_difpor.Text) OR em_difpor.Text = ",0000") THEN
	Messagebox("Atención","Desea ingresar el peso de referencia")
	em_pesref.SetFocus()
ELSE
	IF dw_1.RowCount() > 0 THEN 
		IF Messagebox("Atención","Desea Eliminar el pesaje " + &
							"recien Ingresado?", Question!,YesNo!,2) = 1 THEN
			dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
		END IF
	END IF
	
	Filtra("-1")
	
	em_nropesa.Text		=	""
	em_difpor.Text			=	""
	ddlb_estado.SelectItem ( 1 )
	
	HabilitaEncab(True)
	
	em_nropesa.SetFocus()
END IF
end event

type sle_cadini from singlelineedit within w_pesaje_romana
boolean visible = false
integer x = 795
integer y = 2256
integer width = 873
integer height = 100
integer taborder = 150
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type sle_lectura from singlelineedit within w_pesaje_romana
boolean visible = false
integer x = 41
integer y = 2256
integer width = 713
integer height = 100
integer taborder = 140
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_1 from commandbutton within w_pesaje_romana
integer x = 1701
integer y = 104
integer width = 343
integer height = 92
integer taborder = 90
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Test Lect."
end type

event clicked;Integer	li_factor, li_posini, bufflen
String 	ls_string
Double	ld_kilos

IF This.Text = 'Test Lect.' THEN

	Ole_Puerta.Object.inputlen(istr_puertacomm.LargoLectura) 
	
	bufflen =Ole_Puerta.Object.InBufferCount

	if bufflen > 0 then
		ls_string =  Ole_Puerta.Object.input
	END IF

	li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)
	
	sle_lectura.Visible	=	True
	sle_lectura.Text		=	ls_string
	sle_cadini.Visible	=	True
	sle_cadini.Text		=	'C.Inicio: '+istr_puertacomm.CadenaInicio + &
									' Largo: '+String(Len(istr_puertacomm.CadenaInicio)) + &
									' P.Ini: '+String(li_posini)
	This.Text				=	'Ocultar'								
ELSE
	sle_lectura.Visible	=	False
	sle_cadini.Visible	=	False
	This.Text 				=	'Test Lect.'
END IF
end event

type cb_setup from commandbutton within w_pesaje_romana
integer x = 1102
integer y = 100
integer width = 343
integer height = 92
integer taborder = 80
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Setup Com."
end type

event clicked;//Integer	li_resultado
//
//IF w_maed_movtofrutagranel_recepcion.ib_OCX THEN
//	//Obtiene Seteo de la Puerta
//	ole_puerta.Object.SerialGetPortDefaults (istr_puertacomm.Puerta)     
//	//Permite Seteo al Usuario
//	li_resultado	= ole_puerta.Object.SerialPortSetupDialog(istr_puertacomm.Puerta)
//	//Graba el Seteo
//	If li_resultado = 1 Then 
//		 li_resultado	= ole_puerta.Object.SerialSetPortDefaults(istr_puertacomm.Puerta, "", -1)
//	End If
//END IF	
end event

type em_kilos from editmask within w_pesaje_romana
integer x = 169
integer y = 88
integer width = 667
integer height = 120
integer taborder = 130
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#,##0.00"
end type

type st_fondo from statictext within w_pesaje_romana
integer x = 133
integer y = 52
integer width = 745
integer height = 188
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_pesaje_romana
integer x = 2322
integer y = 1240
integer width = 155
integer height = 132
integer taborder = 120
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Long	ll_fila

IF dw_1.RowCount() > 0 AND (em_difpor.Text = "" OR &
	IsNull(em_difpor.Text) OR em_difpor.Text = ",0000" Or &
	em_pesref.Text = '') Or IsNull(ii_tipo) THEN
	Messagebox("Atención...","Debe ingresar el peso de referencia")
	em_pesref.SetFocus()
ELSE
//	IF dw_1.RowCount() = 0 AND ii_PesajeAnt <> 0 THEN
//		FOR ll_Fila = 1 TO dw_1.FilteredCount()
//			IF dw_1.Object.mfgp_nropes.Filter[ll_Fila] = ii_PesajeAnt THEN
//				dw_1.Object.mfgp_estado.Filter[ll_Fila] = 2
//			ELSE
//				dw_1.Object.mfgp_estado.Filter[ll_Fila] = 1
//			END IF
//		NEXT
//	END IF

	dw_1.SetFilter("")
	dw_1.Filter()
	
	CloseWithReturn(Parent,wstr_pesaje)
END IF
end event

type pb_elimina from picturebutton within w_pesaje_romana
integer x = 2322
integer y = 920
integer width = 155
integer height = 132
integer taborder = 100
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;Integer li_filas, li_inicio

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	li_inicio	=	1//dw_1.RowCount()
	FOR li_filas = li_inicio TO dw_1.RowCount()//(li_inicio - il_bultos) + 1 step -1
		IF dw_1.DeleteRow(0) = 1 THEN
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			MessageBox(Parent.Title,"No se puede borrar actual registro.")
		END IF
	NEXT
	IF dw_1.RowCount() = 0 THEN
		This.Enabled 			= False
		em_bultos.Enabled 	= 	True
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

type pb_inserta from picturebutton within w_pesaje_romana
integer x = 2322
integer y = 740
integer width = 155
integer height = 132
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
end type

event clicked;Integer  li_filas
//
//pb_elimina.Enabled	=	True
//
//il_bultos = Integer(em_bultos.Text)
//IF IsNull(il_bultos) THEN il_bultos = 1
//
//IF istr_puertacomm.pesajebins = 1 AND NOT w_maed_movtofrutagranel_recepcion.ib_OCX THEN
//	il_Fila	=	dw_1.InsertRow(0)
//	
//	dw_1.Object.mfgp_nropes[il_fila]	=	Integer(em_nropesa.Text)
//	dw_1.Object.mfgp_valref[il_Fila]	=	Dec(em_pesref.Text)
//	dw_1.Object.mfgp_estado[il_fila]	=	ii_Estado
//	dw_1.Object.mfgp_horaev[il_fila]	=	Time(Today())	
//	dw_1.Object.mfgp_tippes[il_fila]	=	ii_tipo
//	dw_1.Object.mfgp_secuen[il_fila]	=	il_fila
//	dw_1.SetRow(il_Fila)
//	//dw_1.SetColumn("mfgp_pesore")
//	dw_1.SetColumn("fgmb_nrotar")
//	dw_1.SetFocus()
//ELSE
//	IF Dec(em_kilos.Text) >= istr_puertacomm.PesoMinimo THEN
//		FOR li_filas = 1 to il_bultos
//			il_Fila	=	dw_1.InsertRow(0)
//			
//			dw_1.Object.mfgp_nropes[il_fila]	=	Integer(em_nropesa.Text)
//			dw_1.Object.mfgp_valref[il_Fila]	=	Dec(em_pesref.Text)
//			dw_1.Object.mfgp_estado[il_fila]	=	ii_Estado
//			dw_1.Object.mfgp_horaev[il_fila]	=	Time(Now())
//			dw_1.Object.mfgp_tippes[il_fila]	=	ii_tipo
//			dw_1.Object.mfgp_secuen[il_fila]	=	il_fila	
//			
//			dw_1.Object.mfgp_pesore[il_fila]		=	Dec(em_kilos.Text) / il_bultos
//			dw_1.GroupCalc()
//			em_difpor.Text = String(Calculo(dw_1.Object.total[1], Dec(em_pesref.Text)))
//			ii_pesajenuevo	=	1
//			il_medioseg		=	1
//		NEXT	
//		
//		dw_1.SetRow(il_Fila - il_bultos)
//		dw_1.SetColumn(10)
//		
//	END IF
//END IF
IF dw_1.RowCount() > 0 AND (em_difpor.Text = "" OR &
	IsNull(em_difpor.Text) OR em_difpor.Text = ",0000") THEN
	Messagebox("Atención","Debe ingresar el peso de referencia")
	em_pesref.SetFocus()
ELSE
	IF gstr_paramplanta.binsabins THEN
		FOR li_filas = 1 TO dw_1.RowCount()
			IF IsNull(dw_1.Object.fgmb_nrotar[li_Filas]) THEN 
				Messagebox("Atención","Debe ingresar Número Tarja")
				dw_1.SetColumn("fgmb_nrotar")
				RETURN 1
			ELSEIF IsNull(dw_1.Object.bins_numero[li_Filas]) THEN
				Messagebox("Atención","Debe ingresar Número Bins")
				dw_1.SetColumn("bins_numero")
				RETURN 1
			END IF
		NEXT
	END IF
	
	Filtra("-1")
	
	em_nropesa.Text		=	""
	em_difpor.Text			=	""
	ddlb_estado.SelectItem ( 1 )
	
	HabilitaEncab(True)
	
	il_autoincrement	 	= 	il_autoincrement + 1
	em_nropesa.Text 		= 	String(il_autoincrement)
	em_bultos.Enabled 	= 	True
	em_nropesa.TriggerEvent("Modified")
END IF
end event

type dw_1 from datawindow within w_pesaje_romana
integer x = 37
integer y = 676
integer width = 2176
integer height = 1764
integer taborder = 50
boolean titlebar = true
string title = "Pesaje de Romana"
string dataobject = "dw_pesaje_romana"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event itemerror;RETURN 1
end event

event clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	//This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event itemchanged;String ls_Nula
DataStore			lds_bins

SetNull(ls_Nula)

CHOOSE CASE dwo.name

	CASE "mfgp_pesore"
		dw_1.AcceptText()
		dw_1.GroupCalc()
				
		em_difpor.Text = String(Calculo(dw_1.Object.total[1], Dec(em_pesref.Text)))

	CASE "fgmb_nrotar"
		IF NOT Existetarja(long(data), Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[1])) THEN
			lds_bins	=	Create DataStore
			lds_bins.DataObject = "dw_pesaje_romana"
			lds_bins.SetTransObject(sqlca)
			This.RowsCopy(1, This.FilteredCount(), Filter!, lds_bins, 1, Primary!)
			IF lds_bins.Find("fgmb_nrotar = " + data, 1, lds_bins.RowCount()) > 0 OR &
				This.Find("fgmb_nrotar = " + data, 1, This.RowCount()) > 0 THEN
				MessageBox("Error"," La tarja digitada ya ha sido ingresada")
				This.SetItem(row, "fgmb_nrotar", Integer(ls_nula))
				This.SetColumn("fgmb_nrotar")
				Return 1
			END IF
		ELSE
			This.SetItem(row, "fgmb_nrotar", Long(ls_nula))
			This.SetColumn("fgmb_nrotar")
			RETURN 1
		END IF

	CASE "bins_numero"
		IF Not iuo_bins.Existe(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[1]), long(data), sqlca, TRUE) THEN
			This.SetItem(row, "bins_numero", Integer(ls_nula))
			This.SetColumn("bins_numero")
			RETURN 1
		END IF 
END CHOOSE
end event

type gb_1 from groupbox within w_pesaje_romana
integer x = 2258
integer y = 648
integer width = 288
integer height = 456
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_2 from groupbox within w_pesaje_romana
integer x = 2258
integer y = 1156
integer width = 288
integer height = 272
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_3 from groupbox within w_pesaje_romana
integer x = 1042
integer y = 24
integer width = 462
integer height = 220
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_4 from groupbox within w_pesaje_romana
integer x = 1641
integer y = 24
integer width = 462
integer height = 220
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_5 from groupbox within w_pesaje_romana
integer x = 37
integer y = 232
integer width = 2176
integer height = 408
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
02w_pesaje_romana.bin 
2C00000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff0000000300000000000000000000000000000000000000000000000000000000ccb4798001c6964400000003000000c00000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000260000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000010000003c00000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a000000020000000100000004648a5600101b2c6e0000b6821400000000000000ccb4798001c69644ccb4798001c69644000000000000000000000000fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
23ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00430079007000690072006800670020007400630028002000290039003100340039000000200000000000000000000000000000000000000000000000001234432100000008000003ed000003ed648a560100060000000100000000040000000200000025800008000000000000000000000000003f00000001000000001234432100000008000003ed000003ed648a560100060000000100000000040000000200000025800008000000000000000000000000003f00000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000020000003c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
12w_pesaje_romana.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
