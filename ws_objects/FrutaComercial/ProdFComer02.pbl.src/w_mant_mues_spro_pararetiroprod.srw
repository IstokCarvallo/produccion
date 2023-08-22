$PBExportHeader$w_mant_mues_spro_pararetiroprod.srw
$PBExportComments$Ingreso de retiro de especies en una determinada fecha por un productor
forward
global type w_mant_mues_spro_pararetiroprod from w_mant_directo
end type
type sle_productor from singlelineedit within w_mant_mues_spro_pararetiroprod
end type
type sle_nombre from singlelineedit within w_mant_mues_spro_pararetiroprod
end type
type busca_productor from commandbutton within w_mant_mues_spro_pararetiroprod
end type
type st_1 from statictext within w_mant_mues_spro_pararetiroprod
end type
end forward

global type w_mant_mues_spro_pararetiroprod from w_mant_directo
integer width = 3214
integer height = 2088
string title = "Parametros de Retiro Productor"
sle_productor sle_productor
sle_nombre sle_nombre
busca_productor busca_productor
st_1 st_1
end type
global w_mant_mues_spro_pararetiroprod w_mant_mues_spro_pararetiroprod

type variables
str_variedad		istr_variedad

uo_especie		iuo_especie
uo_variedades	iuo_variedad
uo_categorias	iuo_categoria
uo_Productores	iuo_Productor

Integer              ii_filaultima = 1

DataWindowChild      idwc_variedad, idwc_categoria
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscaproductor ()
public function boolean fechatemporada (date ai_fechaini)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_Especie, ls_Variedad, ls_Categoria

ls_Especie  	=	String(dw_1.Object.espe_codigo[il_Fila])
ls_Variedad 	=	String(dw_1.Object.vari_codigo[il_Fila])
ls_Categoria	=	String(dw_1.Object.cate_codigo[il_Fila])

CHOOSE CASE as_Columna
	CASE "espe_codigo"
		ls_Especie  	=	as_Valor

	CASE "vari_codigo"
		ls_Variedad 	=	as_Valor
		
	CASE "Cate_codigo"
		ls_Categoria 	=	as_Valor		

END CHOOSE

ll_Fila	=	dw_1.Find("espe_codigo = " + ls_Especie + " AND " + &
							"vari_codigo = " + ls_Variedad + " AND " + &
							"cate_codigo = " + ls_Categoria , &
							1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaproductor ();str_busqueda	lstr_busq
String				ls_Null

SetNull(ls_Null)

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	sle_productor.SetFocus()
	sle_Productor.Text	=	ls_Null
	sle_Nombre.Text		=	ls_Null
	dw_1.SetFocus()
ELSE
	IF Not iuo_Productor.Rechazado(long(lstr_Busq.Argum[1]), False, Sqlca) THEN
		sle_Productor.Text		=	lstr_busq.argum[1]	
		sle_Nombre.Text   		=	lstr_busq.argum[2]
		Istr_Mant.Argumento[1]	=  lstr_busq.argum[1]
		pb_lectura.enabled		=	True
	ELSE
		MessageBox("Error", "El Productor se encuentra Rechazado, Ingrese" + &
						"otro Código de Productor", Information!, Ok!)
		pb_lectura.enabled =	False						
		sle_productor.text = ls_Null
		sle_nombre.text	 = ls_Null
	END IF
END IF
end subroutine

public function boolean fechatemporada (date ai_fechaini);Integer  tempor

SELECT	pate_tempor
INTO		:tempor
FROM		dbo.paramtemporada        
WHERE    :ai_fechaini  Between pate_inicio and pate_termin
AND		pate_vigent = 1;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ParamTemporada")
	RETURN FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

on w_mant_mues_spro_pararetiroprod.create
int iCurrent
call super::create
this.sle_productor=create sle_productor
this.sle_nombre=create sle_nombre
this.busca_productor=create busca_productor
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_productor
this.Control[iCurrent+2]=this.sle_nombre
this.Control[iCurrent+3]=this.busca_productor
this.Control[iCurrent+4]=this.st_1
end on

on w_mant_mues_spro_pararetiroprod.destroy
call super::destroy
destroy(this.sle_productor)
destroy(this.sle_nombre)
destroy(this.busca_productor)
destroy(this.st_1)
end on

event ue_recuperadatos;Long	ll_fila, respuesta, ll_Productor 

ll_Productor = long(istr_mant.Argumento[1])

DO
	ll_fila	= dw_1.Retrieve(ll_Productor)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		
		il_fila = 1
		ii_filaultima=1
		dw_1.getchild("vari_codigo",idwc_variedad)
		idwc_variedad.SettransObject(SQLCA)
//		IF idwc_variedad.Retrieve(dw_1.Object.espe_codigo[il_fila]) = 0 THEN
//			idwc_variedad.InsertRow(0)
//		END IF
		
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		pb_insertar.Enabled = True
		pb_insertar.SetFocus()		
		
	ELSE
		pb_insertar.Enabled = True
		pb_insertar.SetFocus()
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_antesguardar;
FOR il_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! OR dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		dw_1.SetItem(il_fila,"prod_codigo", Long(istr_mant.argumento[1]))
		TriggerEvent("ue_validaregistro")
	END IF
NEXT
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEspecie"
END IF

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nVariedad"
END IF

IF Isnull(dw_1.Object.cate_codigo[il_fila]) OR dw_1.Object.cate_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCategoria"
END IF

//IF Isnull(dw_1.Object.prep_fecini[il_fila]) THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nFecha Inicio"
//END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
   dw_1.SetFocus()
	Message.DoubleParm = -1
	Return
	
END IF
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db()  THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

end event

event ue_nuevo();call super::ue_nuevo;pb_grabar.enabled=true
dw_1.SetColumn("espe_codigo")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "PARAMETROS DE RETIRO PRODUCTOR"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_spro_pararetiroprod"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2470
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono


dw_1.getchild("vari_codigo",idwc_variedad)
idwc_variedad.SettransObject(SQLCA)
IF idwc_variedad.Retrieve(0,0) = 0 THEN
	idwc_variedad.InsertRow(0)
END IF	

dw_1.getchild("cate_codigo",idwc_categoria)
idwc_categoria.SettransObject(SQLCA)
IF idwc_categoria.Retrieve() = 0 THEN
	idwc_categoria.InsertRow(0)
ELSE
	idwc_categoria.SetSort("cate_nombre A")
	idwc_categoria.Sort()

	idwc_categoria.SetFilter("cate_embala <> 1")
	idwc_categoria.Filter()
END IF	

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
								
buscar			= "Especie:Nespe_codigo,Variedad:Nvari_codigo"
ordenar			= "Especie:espe_codigo,Variedad:vari_codigo"
is_ultimacol	= "prep_fecini"

iuo_especie  	=	Create uo_especie
iuo_variedad 	=	Create uo_variedades
iuo_categoria	=	Create uo_categorias
iuo_Productor	=	Create uo_Productores

sle_productor.Setfocus()
pb_lectura.enabled = False
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_pararetiroprod
integer width = 2569
integer height = 240
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 420
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;String ls_Null

Setnull(ls_Null)

Istr_mant.Argumento[1] = ""
Sle_Productor.text 	  = ls_Null
Sle_Nombre.text 		  = ls_Null
pb_lectura.enabled 	  = False
pb_insertar.enabled	  = False
Sle_Productor.Setfocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 124
integer taborder = 30
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 780
integer taborder = 70
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 600
integer taborder = 60
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 1524
integer taborder = 100
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 1140
integer taborder = 90
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_pararetiroprod
integer x = 2752
integer y = 960
integer taborder = 80
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_pararetiroprod
integer y = 384
integer width = 2569
integer height = 1528
integer taborder = 40
string dataobject = "dw_mues_spro_pararetiroprod"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Null

SetNull(ls_Null)

ls_columna  = dwo.Name

CHOOSE Case ls_columna
	Case "espe_codigo"
		If Not iuo_Especie.Existe(integer(data),True,SQLCA) Or Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			dw_1.getchild("vari_codigo",idwc_variedad)
			idwc_variedad.SettransObject(SQLCA)
			idwc_variedad.Retrieve(iuo_Especie.Codigo)
		End If
		
	Case "vari_codigo"
			If Duplicado(ls_Columna, Data) Or Not iuo_Variedad.Existe(iuo_Especie.Codigo, Long(Data), True, Sqlca)Then
				dw_1.SetItem(il_fila,"vari_codigo",Integer(ls_Null))
				dw_1.SetItem(il_fila,"vari_nombre",ls_Null)
				Return 1
			Else
				dw_1.Object.vari_nombre[il_fila]=iuo_variedad.Nombrevariedad	
			End If
			
	Case "cate_codigo"
		If Duplicado(ls_Columna, Data) Or Not iuo_categoria.existe(integer(data),True,SQLCA) Then
			dw_1.SetItem(il_fila,"cate_codigo",Integer(ls_Null))
			Return 1
		End If
		
	Case "prep_fecini"
			If Not FechaTemporada(Date(data)) Then
				Messagebox("Error de Consistencia","Fecha fuera de Temporada")
				dw_1.SetItem(il_fila,"prep_fecini",Date(ls_Null))
				Return 1
			End If

End CHOOSE


end event

event dw_1::clicked;call super::clicked;IF RowCount()>0 AND ii_filaultima<>il_fila THEN		
	IF idwc_variedad.Retrieve(dw_1.Object.espe_codigo[il_fila]) = 0 THEN
		idwc_variedad.InsertRow(0)
	END IF
   ii_filaultima=il_fila	
END IF
end event

type sle_productor from singlelineedit within w_mant_mues_spro_pararetiroprod
integer x = 526
integer y = 156
integer width = 279
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 5
borderstyle borderstyle = stylelowered!
end type

event modified;String ls_Null

SetNull(ls_Null)

IF Not iuo_Productor.Existe(Long(Sle_productor.Text), True, Sqlca) THEN
	Messagebox("Error de Consistencia"," El Productor no existe por favor "+&
				 "ingrese otro Código de Productor")
	Sle_Productor.Text = ls_Null
	Sle_Productor.Setfocus()
	Return
ELSE
	IF iuo_Productor.Rechazado(Long(Sle_productor.Text), False, Sqlca) THEN
		Messagebox("Error de Consistencia"," El Productor se ecuentra rechazado "+&
	   "ingrese otro Código Productor")
		Sle_Productor.Text = ls_Null
		Sle_Nombre.Text	 = ls_Null
		pb_lectura.enabled = False
		Sle_Productor.Setfocus()
		Return
	ELSE
		istr_mant.Argumento[1] = string(sle_Productor.Text)
		pb_lectura.enabled = True
	END IF
END IF
end event

type sle_nombre from singlelineedit within w_mant_mues_spro_pararetiroprod
integer x = 965
integer y = 148
integer width = 1609
integer height = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type busca_productor from commandbutton within w_mant_mues_spro_pararetiroprod
string tag = "Busca Productores"
integer x = 837
integer y = 156
integer width = 96
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;buscaproductor()
end event

type st_1 from statictext within w_mant_mues_spro_pararetiroprod
integer x = 215
integer y = 156
integer width = 302
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

