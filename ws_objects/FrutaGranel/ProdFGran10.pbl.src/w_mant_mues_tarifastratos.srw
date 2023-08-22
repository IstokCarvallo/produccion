$PBExportHeader$w_mant_mues_tarifastratos.srw
forward
global type w_mant_mues_tarifastratos from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_tarifastratos
end type
type st_2 from statictext within w_mant_mues_tarifastratos
end type
type dw_especie from datawindow within w_mant_mues_tarifastratos
end type
type st_3 from statictext within w_mant_mues_tarifastratos
end type
type em_fecha from editmask within w_mant_mues_tarifastratos
end type
type ddlb_codigo from dropdownlistbox within w_mant_mues_tarifastratos
end type
end forward

global type w_mant_mues_tarifastratos from w_mant_directo
integer width = 2382
integer height = 1832
string title = "TARIFAS TRATOS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
st_1 st_1
st_2 st_2
dw_especie dw_especie
st_3 st_3
em_fecha em_fecha
ddlb_codigo ddlb_codigo
end type
global w_mant_mues_tarifastratos w_mant_mues_tarifastratos

type variables
DataWindowChild 						idwc_especie, idwc_embalaje
Date		id_fecha
Integer	il_codigo

end variables

forward prototypes
public function boolean existeespecie (integer ai_especie)
public function boolean duplicado (string as_embalaje)
public function boolean existeembalaje (integer ai_especie, string as_embalaje)
end prototypes

public function boolean existeespecie (integer ai_especie);Integer li_count

SELECT count(*)
INTO :li_count  
FROM  dba.especies
WHERE espe_codigo = :ai_especie;

IF (sqlca.SQLCode) = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Especies")
	RETURN False
	
ELSEIF li_count < 1 THEN
	MessageBox("Atención","Código de Especie no existe.", Exclamation!, Ok!)
	RETURN False
END IF
RETURN TRUE
end function

public function boolean duplicado (string as_embalaje);String ls_embalaje, ls_fecha
Long  ll_fila
Integer	 li_especie, li_codigo

li_especie = Integer(istr_mant.argumento[2])
li_codigo  = Integer(istr_mant.argumento[1])
ls_embalaje = as_embalaje
ls_fecha		= em_fecha.Text

ll_Fila	=	dw_1.Find("emba_codigo = '" + ls_embalaje + "'" ,1, dw_1.RowCount())
	

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existeembalaje (integer ai_especie, string as_embalaje);Integer li_count

SELECT count(*)
INTO :li_count  
FROM  dba.embalajes
WHERE espe_codigo = :ai_especie
AND	emba_codigo = :as_embalaje;

IF (sqlca.SQLCode) = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla EmbalajesProd")
	RETURN True
	
ELSEIF li_count < 1 THEN
	MessageBox("Atención","Código de Embalaje no existe.", Exclamation!, Ok!)
	RETURN True
END IF
RETURN False
end function

on w_mant_mues_tarifastratos.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.dw_especie=create dw_especie
this.st_3=create st_3
this.em_fecha=create em_fecha
this.ddlb_codigo=create ddlb_codigo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_especie
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_fecha
this.Control[iCurrent+6]=this.ddlb_codigo
end on

on w_mant_mues_tarifastratos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.st_3)
destroy(this.em_fecha)
destroy(this.ddlb_codigo)
end on

event open;call super::open;dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

dw_1.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
idwc_embalaje.Retrieve(gi_CodEspecie)
idwc_embalaje.Sort()


em_fecha.Text = String(Today())

istr_mant.Argumento[1]	=	String(gi_CodExport)


end event

event ue_recuperadatos;call super::ue_recuperadatos;id_fecha = Date(em_fecha.Text)

dw_1.Retrieve(Dec(istr_mant.argumento[1]),Dec(istr_mant.argumento[2]),id_fecha)

ddlb_codigo.Enabled 	=	False
dw_especie.Enabled	=	False
em_fecha.Enabled	=	False



end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.tatr_codigo[il_fila]	=	Integer(istr_mant.Argumento[1])
dw_1.Object.espe_codigo[il_fila]	=	Integer(istr_mant.Argumento[2])
dw_1.Object.tatr_fectra[il_fila]	=	Date(id_fecha)

dw_1.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
idwc_embalaje.Retrieve(Integer(istr_mant.Argumento[2]))
idwc_embalaje.Sort()
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "TARIFA TRATOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_spro_tarifatratos"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Dec(istr_mant.argumento[1]),Dec(istr_mant.argumento[2]),id_fecha)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_Contador
String	ls_Mensaje, ls_Columna[]


IF IsNull(dw_1.Object.espe_codigo[il_Fila]) OR dw_1.Object.espe_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEspecie"
	ls_Columna[li_Contador]	=	"espe_codigo"
END IF

IF IsNull(dw_1.Object.tatr_codigo[il_Fila]) OR dw_1.Object.tatr_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nTipo codigo"
	ls_Columna[li_Contador]	=	"tatr_codigo"
END IF

IF IsNull(dw_1.Object.emba_codigo[il_Fila]) OR dw_1.Object.emba_codigo[il_Fila] = "" THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEmbalaje"
	ls_Columna[li_Contador]	=	"emba_codigo"
END IF

//IF IsNull(dw_1.Object.tatr_fectra[il_Fila]) OR dw_1.Object.tatr_fectra[il_Fila] = '1900-01-01' THEN
//	li_Contador ++
//	ls_Mensaje 					+=	"~nFecha"
//	ls_Columna[li_Contador]	=	"tatr_fectra"
//END IF


IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_tarifastratos
integer y = 32
integer width = 1861
integer height = 376
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_tarifastratos
integer x = 2089
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;ddlb_codigo.Enabled 	=	True
dw_especie.Enabled	=	True
em_fecha.Enabled	=	True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_tarifastratos
integer x = 2089
end type

event pb_lectura::clicked;IF Isnull(dw_especie.Object.espe_codigo[1]) OR dw_especie.Object.espe_codigo[1] = 0 OR &
	isnull(em_fecha.Text) OR em_fecha.Text = '' OR isnull(il_codigo) OR il_codigo = 0 THEN
	MessageBox("Atención","Faltan datos en el encabezado.", Exclamation!, Ok!)
	RETURN 
ELSE
	istr_mant.argumento[2] = String(dw_especie.Object.espe_codigo[1])
	istr_mant.argumento[3] = String(em_fecha.Text)
	Parent.PostEvent("ue_recuperadatos")
END IF	
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_tarifastratos
integer x = 2089
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_tarifastratos
integer x = 2089
integer taborder = 70
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_tarifastratos
integer x = 2089
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_tarifastratos
integer x = 2089
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_tarifastratos
integer x = 2094
integer taborder = 90
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_tarifastratos
integer x = 73
integer y = 428
integer width = 1861
integer height = 1164
integer taborder = 60
string dataobject = "dw_mues_spro_tarifatratos"
end type

event dw_1::itemchanged;call super::itemchanged;Integer 	li_null, li_TipoEnvase, li_envase
String	ls_columna

SetNull(li_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "emba_codigo"
		IF existeembalaje(Integer(istr_mant.argumento[2]),data) THEN
			dw_1.SetItem(row, "emba_codigo", String(li_null))
			RETURN 1
		END IF
		
		IF Duplicado(data) THEN
			dw_1.SetItem(row, "emba_codigo", String(li_null))
			RETURN 1
		
		END IF	
		
	CASE "tatr_valors"	
		IF dw_1.Object.tatr_valors[row] <> 0 OR NOT isnull(dw_1.Object.tatr_valors[row]) THEN
			pb_grabar.Enabled = True
		END IF		
END CHOOSE

end event

event dw_1::clicked;//
end event

type st_1 from statictext within w_mant_mues_tarifastratos
integer x = 242
integer y = 60
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Código"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_tarifastratos
integer x = 242
integer y = 308
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_mant_mues_tarifastratos
integer x = 663
integer y = 168
integer width = 983
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

IF existeespecie(Integer(Data)) THEN
	istr_mant.argumento[2] 	= 	data
ELSE
	This.SetItem(1, "espe_codigo", Integer(ll_null))

	RETURN 1
END IF
	




end event

event itemerror;Return 1
end event

type st_3 from statictext within w_mant_mues_tarifastratos
integer x = 242
integer y = 184
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_tarifastratos
integer x = 663
integer y = 284
integer width = 407
integer height = 88
integer taborder = 40
boolean bringtotop = true
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type ddlb_codigo from dropdownlistbox within w_mant_mues_tarifastratos
integer x = 663
integer y = 52
integer width = 677
integer height = 400
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"SELECCIONADORA","PESAJE","EMBALAJE"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[1] = String(Index)
il_codigo = Index
end event

