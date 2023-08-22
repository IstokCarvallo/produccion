$PBExportHeader$w_datos_contenedor.srw
forward
global type w_datos_contenedor from window
end type
type dw_1 from uo_dw within w_datos_contenedor
end type
type pb_salir from picturebutton within w_datos_contenedor
end type
end forward

global type w_datos_contenedor from window
integer width = 3131
integer height = 668
boolean titlebar = true
string title = "Datos Contenedor"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar pbm_custom11
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_1 dw_1
pb_salir pb_salir
end type
global w_datos_contenedor w_datos_contenedor

type variables
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantInspec
Date				id_FechaRepa
Date		id_FechaAcceso
Time		it_HoraAcceso

str_mant	istr_mant


end variables

forward prototypes
public function boolean buscasellos (long sello)
end prototypes

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean buscasellos (long sello);Long		ll_numero, ll_desde, ll_hasta, ll_Null, ll_posici, ll_largosello, ll_sello
Boolean	lb_retorno=True
Integer	li_Planta, li_cansel, li_encuentra=0, li_haya
String	ls_Sellos, ls_Null

SetNull(ls_Null)
SetNull(ll_Null)

li_Planta	=	dw_1.Object.plde_codigo[1]
ls_Sellos	=	dw_1.Object.defe_numsel[1]
li_cansel	=	dw_1.Object.defe_cansel[1]

IF IsNull(ls_Sellos) THEN
	ls_Sellos=''
END IF

IF IsNull(li_cansel) THEN
	li_cansel=0
END IF

IF li_cansel > 0 THEN
	
	ll_largosello		=	len(ls_Sellos)
	ll_sello			=	len(String(sello))
	ll_sello			=	ll_sello+4
	
	For ll_posici = 1 To ll_largosello
		
		 IF Mid(ls_Sellos,ll_posici,1)='-' THEN li_encuentra++
	  	 IF Mid(ls_Sellos,ll_posici,ll_sello)='- '+String(sello)+' -' THEN li_haya++
			
	Next
	
	IF li_encuentra >= li_cansel THEN
			MessageBox("Error","Cantidad de Sellos Registrados Supera los Informados",Information!, Ok!)
			RETURN False
	END IF
	IF li_haya > 0 THEN
			MessageBox("Error","Número de Sello Duplicado",Information!, Ok!)
			RETURN False
	END IF

END IF

SELECT sell_inicio, sell_termin
  INTO :ll_desde, :ll_hasta
  FROM DBA.correlsellos
  WHERE plde_codigo = :li_planta
  AND   sell_vigenc = 0;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Correlsellos")
	lb_retorno = False
ELSEIF sqlca.SQLCode = 0 THEN
	
	 IF sello < ll_desde OR sello > ll_hasta THEN
			MessageBox("Error","Sello Fuera de Rango de Control",Information!, Ok!)
			lb_retorno = False
	 ELSE
		 ls_Sellos	=	ls_Sellos + " - " + String(sello)
		 dw_1.Object.defe_numsel[1]	=	ls_Sellos
		 dw_1.Object.sello[1]			=  ll_Null
					 
		 lb_retorno = True
	 END IF

ELSE
	lb_retorno = False
END IF

RETURN lb_retorno
end function

on w_datos_contenedor.create
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.Control[]={this.dw_1,&
this.pb_salir}
end on

on w_datos_contenedor.destroy
destroy(this.dw_1)
destroy(this.pb_salir)
end on

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

end event

type dw_1 from uo_dw within w_datos_contenedor
integer x = 55
integer y = 20
integer width = 2610
integer height = 500
integer taborder = 10
string dataobject = "dw_mant_despafrigoen_contenedor"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;Long		ll_null, ll_Planilla, ll_tpcont, ll_orcont, ll_guides, ll_sello, ll_despacho
String	ls_columna, ls_null, ls_embq_codigo, ls_TipoPla, ls_digito, ls_digito1,&
			ls_glosa,ls_glosag,ls_ubisel,ls_numsel,ls_nrcont,ls_patent,ls_pataco, ls_chofer, ls_rutchofer, ls_sagmultipuerto
Date		ld_nula
Integer	li_Cliente, li_Planta, li_existe, li_espmul, li_sicodigo, li_trancodigo, li_ticacodigo, li_tipocontenedor

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "defe_nrcont"
		
		ls_digito 	= f_digito_verifica_contene(data) 
		IF ls_digito = '10' THEN
			ls_digito = '0'
		END IF	
		
		ls_digito1 	= mid(data,11,1)
		
		IF Upper(ls_digito) <> Upper(ls_digito1) THEN
			MessageBox("Atención","Número Contenedor Esta Incorrecto, Digite Otro Número.")
			This.SetItem(row,'defe_nrcont',ls_null)
			RETURN 1
		END IF	
	
	CASE "defe_orcont"
	
	CASE "sello"		
		
		ll_sello	=	Long(data)
		
		IF buscasellos(ll_sello) = False THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF
		
END CHOOSE
end event

type pb_salir from picturebutton within w_datos_contenedor
integer x = 2784
integer y = 212
integer width = 300
integer height = 245
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
end type

event clicked;CloseWithReturn(Parent, istr_mant3)

end event

