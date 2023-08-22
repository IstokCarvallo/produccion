$PBExportHeader$w_mant_mues_distribucioncalibre.srw
forward
global type w_mant_mues_distribucioncalibre from w_mant_directo
end type
type uo_selproductor from uo_seleccion_productor within w_mant_mues_distribucioncalibre
end type
type uo_selpredio from uo_seleccion_prodpredio within w_mant_mues_distribucioncalibre
end type
type uo_selcuartel from uo_seleccion_prodcuarteles within w_mant_mues_distribucioncalibre
end type
type st_1 from statictext within w_mant_mues_distribucioncalibre
end type
type st_2 from statictext within w_mant_mues_distribucioncalibre
end type
type st_3 from statictext within w_mant_mues_distribucioncalibre
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_distribucioncalibre
end type
type uo_selvariedad from uo_seleccion_variedad within w_mant_mues_distribucioncalibre
end type
type st_4 from statictext within w_mant_mues_distribucioncalibre
end type
type st_5 from statictext within w_mant_mues_distribucioncalibre
end type
end forward

global type w_mant_mues_distribucioncalibre from w_mant_directo
integer width = 3278
uo_selproductor uo_selproductor
uo_selpredio uo_selpredio
uo_selcuartel uo_selcuartel
st_1 st_1
st_2 st_2
st_3 st_3
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
st_4 st_4
st_5 st_5
end type
global w_mant_mues_distribucioncalibre w_mant_mues_distribucioncalibre

type variables

end variables

forward prototypes
public function boolean wf_validaporcentaje (string as_columna, string as_valor)
public function integer wf_recupera (ref datawindow adw, long productor, integer predio, integer cuartel, integer especie, integer variedad)
end prototypes

public function boolean wf_validaporcentaje (string as_columna, string as_valor);Boolean	lb_Retorno

dw_1.AcceptText()

If dw_1.Object.Total[1] > 100 Then
	MessageBox("", "Se sobrepaso el 100%")
	lb_Retorno = False
Else
	lb_Retorno = True
End If

Return lb_Retorno
end function

public function integer wf_recupera (ref datawindow adw, long productor, integer predio, integer cuartel, integer especie, integer variedad);Long    ll_fila, ll_Find, ll_New
String  ls_Find, ls_Columna
Integer	li_Codigo
String		ls_Codigo

DataStore	ds_Carga

ds_Carga	=	Create DataStore

adw.Retrieve(Productor, Predio, Cuartel, Especie, Variedad)

ds_Carga.Dataobject =	'dw_carga_pronvariecalibre'
ds_Carga.SetTransObject(SQLca)
ll_fila	=	ds_Carga.Retrieve(Especie, Variedad)

FOR ll_fila = 1 to ds_Carga.RowCount()
	ls_Find 		= "vaca_calibr = "
	ls_Columna	= "vaca_calibr"
	ls_Codigo	=	ds_Carga.Object.vaca_calibr[ll_Fila]
	ll_Find		=	adw.Find(ls_Find + "'" + ls_Codigo + "'", 1, adw.RowCount())
	
	If ll_Find = 0 Then
		ll_New = adw.InsertRow(0)
		adw.Object.prod_codigo[ll_New]	= Productor
		adw.Object.prpr_codigo[ll_New]	= Predio
		adw.Object.prcc_codigo[ll_New]	= Cuartel
		adw.SetItem(ll_New, ls_Columna, ls_Codigo)
		adw.Object.espe_codigo[ll_New]	= Especie
		adw.Object.vari_codigo[ll_New]	= Variedad
	End If
Next

Destroy ds_Carga
	 
Return adw.RowCount()
end function

on w_mant_mues_distribucioncalibre.create
int iCurrent
call super::create
this.uo_selproductor=create uo_selproductor
this.uo_selpredio=create uo_selpredio
this.uo_selcuartel=create uo_selcuartel
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.st_4=create st_4
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selproductor
this.Control[iCurrent+2]=this.uo_selpredio
this.Control[iCurrent+3]=this.uo_selcuartel
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.uo_selespecie
this.Control[iCurrent+8]=this.uo_selvariedad
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.st_5
end on

on w_mant_mues_distribucioncalibre.destroy
call super::destroy
destroy(this.uo_selproductor)
destroy(this.uo_selpredio)
destroy(this.uo_selcuartel)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.st_4)
destroy(this.st_5)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPredio.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCuartel.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelProductor.Seleccion(False, False)
	uo_SelPredio.Seleccion(False, False)
	uo_SelCuartel.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(False, False)
	
	uo_SelProductor.Filtra(-1)
	
	This.Title = "DISTRIBUCION CALIBRES"
	dw_1.DataObject = "dw_mant_mues_calibrecuartel"
	dw_1.SetTransObject(SQLCA)
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= wf_Recupera(dw_1, uo_SelProductor.Codigo, uo_SelPredio.Codigo, uo_SelCuartel.Codigo, &
										uo_SelEspecie.Codigo, uo_SelVariedad.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		pb_imprimir.Enabled	= True
		il_fila						= 1
		
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
		ias_campo[1]			= ""
		ias_campo[2]			= ""
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;istr_mant.argumento[1]	=	""
dw_1.SetColumn("calm_codigo")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String	ls_argumento, ls_argum1
str_info	lstr_info

lstr_info.titulo	= "DISTRIBUCION CALIBRES"
lstr_info.copias	= 1
OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_calibrecuartel"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelProductor.Codigo, uo_SelPredio.Codigo, uo_SelCuartel.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	IF Not gs_Ambiente = 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, lstr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return
IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar();call super::ue_antesguardar;IF il_fila > 0 THEN
	//Chequeo de última fila registrada en caso de mantención.
	TriggerEvent("ue_validaregistro")
END IF
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
		il_fila = 0
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_distribucioncalibre
integer x = 91
integer y = 72
integer width = 2683
integer height = 468
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_distribucioncalibre
integer x = 2853
integer y = 408
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_distribucioncalibre
integer x = 2843
integer y = 172
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_distribucioncalibre
integer x = 2843
integer y = 764
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_distribucioncalibre
boolean visible = false
integer x = 2843
integer y = 584
integer taborder = 50
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_distribucioncalibre
integer x = 2843
integer y = 1440
integer taborder = 130
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_distribucioncalibre
integer x = 2834
integer y = 1188
integer taborder = 120
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_distribucioncalibre
integer x = 2843
integer y = 968
integer taborder = 110
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_distribucioncalibre
integer x = 91
integer y = 564
integer width = 2683
integer height = 1252
integer taborder = 40
string dataobject = "dw_mant_mues_calibrecuartel"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Null, ls_Columna

SetNull(ls_Null)
ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	//CASE "cacu_pordis", "cocu_pordis"
	CASE "cocu_pordis"
		If Not wf_ValidaPorcentaje(ls_Columna, Data) Then
			dw_1.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If	
		
END CHOOSE
end event

event dw_1::dwnkey;//
end event

type uo_selproductor from uo_seleccion_productor within w_mant_mues_distribucioncalibre
integer x = 521
integer y = 128
integer height = 92
integer taborder = 60
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPredio.Filtra(-1)
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)
		
End Choose
end event

type uo_selpredio from uo_seleccion_prodpredio within w_mant_mues_distribucioncalibre
integer x = 521
integer y = 232
integer height = 92
integer taborder = 70
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_prodpredio::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCuartel.Filtra(-1, -1)
		
	Case Else
		uo_SelCuartel.Filtra(uo_SelProductor.Codigo, This.Codigo)
		
End Choose
end event

type uo_selcuartel from uo_seleccion_prodcuarteles within w_mant_mues_distribucioncalibre
integer x = 521
integer y = 336
integer height = 180
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcuartel.destroy
call uo_seleccion_prodcuarteles::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	CAse -1, -9
			uo_SelEspecie.LimpiarDatos()
			uo_SelVariedad.LimpiarDatos()

	Case Else
			uo_SelEspecie.Inicia(This.Especie)
			uo_SelVariedad.Filtra(This.Especie)
			uo_SelVariedad.Inicia(This.Variedad)
			
	End Choose
end event

type st_1 from statictext within w_mant_mues_distribucioncalibre
integer x = 160
integer y = 140
integer width = 320
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_distribucioncalibre
integer x = 160
integer y = 244
integer width = 320
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_distribucioncalibre
integer x = 160
integer y = 348
integer width = 320
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
string text = "Cuartel"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_distribucioncalibre
integer x = 1833
integer y = 128
integer height = 92
integer taborder = 90
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	CAse -1, -9
			uo_SelVariedad.LimpiarDatos()

	Case Else
			uo_SelVariedad.Filtra(This.Codigo)
			
	End Choose
end event

type uo_selvariedad from uo_seleccion_variedad within w_mant_mues_distribucioncalibre
integer x = 1833
integer y = 232
integer height = 92
integer taborder = 100
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_4 from statictext within w_mant_mues_distribucioncalibre
integer x = 1472
integer y = 144
integer width = 320
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within w_mant_mues_distribucioncalibre
integer x = 1472
integer y = 248
integer width = 320
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
string text = "Variedad"
boolean focusrectangle = false
end type

