$PBExportHeader$w_info_lotes_duchados.srw
$PBExportComments$Ventana Informe de Lotes Duchados.
forward
global type w_info_lotes_duchados from w_para_informes
end type
type dw_1 from datawindow within w_info_lotes_duchados
end type
end forward

global type w_info_lotes_duchados from w_para_informes
integer x = 521
integer y = 656
integer width = 2455
integer height = 948
string title = "LOTES DUCHADOS"
dw_1 dw_1
end type
global w_info_lotes_duchados w_info_lotes_duchados

type variables
Str_mant				istr_mant
Integer				ii_Ducha, ii_Estanque, ii_expo
Date					id_Fecha
Time				 	it_Hora


Datawindowchild   idwc_expo
uo_duchacontrol	iuo_duchacontrol
end variables

forward prototypes
public subroutine buscaducha ()
public function boolean existeducha (string as_columna, string as_valor)
end prototypes

public subroutine buscaducha ();Str_Busqueda	lstr_busq

OpenWithParm(w_busc_duchacontrol, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1]		=	lstr_busq.argum[1]
	istr_mant.argumento[2]		=	lstr_busq.argum[2]
	istr_mant.argumento[3]		=	lstr_busq.argum[3]
	istr_mant.argumento[4]		=	lstr_busq.argum[4]
	
	ii_Ducha							=	Integer(lstr_busq.Argum[1])
	ii_Estanque						=	Integer(lstr_busq.Argum[2])
	id_Fecha							=	Date(Mid(lstr_busq.Argum[3],1,10))
	it_Hora							=	Time(Mid(lstr_busq.Argum[4],12))

	dw_1.Object.duch_codigo[1]	=	ii_Ducha
	dw_1.Object.codu_nropos[1]	=	ii_Estanque
	dw_1.Object.codu_fecini[1]	=	id_Fecha
	dw_1.Object.codu_horini[1]	=	it_Hora
END IF
end subroutine

public function boolean existeducha (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Ducha, li_Estanque, li_Cantidad
Date		ld_Fecha, ld_FechaNula
Time	 	lt_Hora

li_Ducha		= dw_1.Object.duch_codigo[1]
li_Estanque	= dw_1.Object.codu_nropos[1]
ld_Fecha		= dw_1.Object.codu_fecini[1]
lt_Hora		= dw_1.Object.codu_horini[1]

CHOOSE CASE as_Columna
	CASE "duch_codigo"
		li_Ducha		=	Integer(as_Valor)
		
	CASE "codu_nropos"
		li_Estanque	=	Integer(as_Valor)
		
	CASE "codu_fecini"
		ld_Fecha		=	Date(as_Valor)		
	CASE "codu_horini"
		lt_Hora		=	Time(as_Valor)
		
END CHOOSE

IF NOT IsNull(li_Ducha) AND NOT IsNull(li_Estanque) AND NOT IsNull(ld_Fecha) AND &
	NOT IsNull(lt_Hora) THEN
	IF NOT iuo_duchacontrol.Existe(SqlCa,li_Ducha,li_Estanque,ld_Fecha,lt_Hora,True) THEN
		lb_Retorno = False
	ELSE
		ii_Ducha		=	li_Ducha
		ii_Estanque	=	li_Estanque
		id_Fecha	=	ld_Fecha
		it_Hora		=	lt_Hora

		lb_Retorno = True
	END IF
END IF

lb_Retorno = False

RETURN lb_Retorno
end function

on w_info_lotes_duchados.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_lotes_duchados.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;dw_1.GetChild("exportador",idwc_expo)
idwc_expo.SetTransObject(SQLCA)
idwc_expo.Retrieve()

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

iuo_duchacontrol	=	Create uo_duchacontrol

dw_1.SetItem(1,"exportador",gi_codexport)
ii_expo = gi_codexport

dw_1.SetFocus()

dw_1.SetColumn("duch_codigo")
end event

type st_titulo from w_para_informes`st_titulo within w_info_lotes_duchados
integer x = 105
integer y = 68
integer width = 1856
string text = "Informe de Lotes Duchados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_lotes_duchados
integer x = 2098
integer y = 196
integer taborder = 30
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "LOTES DUCHADOS"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_spro_lotesduchados"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(ii_Ducha, ii_Estanque, id_Fecha, it_Hora,ii_expo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_lotes_duchados
integer x = 2098
integer y = 560
integer taborder = 40
end type

type dw_1 from datawindow within w_info_lotes_duchados
integer x = 105
integer y = 248
integer width = 1856
integer height = 460
integer taborder = 40
boolean bringtotop = true
string dataobject = "dw_info_lotes_duchados"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event buttonclicked;
CHOOSE CASE dwo.Name
	CASE "buscaducha"

		BuscaDucha()

END CHOOSE
end event

event itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "duch_codigo", "codu_nropos", "codu_fecini", "codu_horini"
	   ExisteDucha(ls_Columna, Data)

   CASE "exportador"
		ii_expo = integer(data)
END CHOOSE
end event

